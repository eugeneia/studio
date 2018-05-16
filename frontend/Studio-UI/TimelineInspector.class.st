Class {
	#name : #TimelineInspector,
	#superclass : #GTInspector,
	#instVars : [
		'summaryDir',
		'processDir'
	],
	#category : #'Studio-UI'
}

{ #category : #'instance creation' }
TimelineInspector class >> openOn: dir [
	| inspector |
	inspector := self new.
	inspector processDir: dir asFileReference.
	inspector
		showFirst: [ :composite |
			composite text
				title: 'R script';
				populate: #selection iconName: #glamorousGo on: $G entitled: 'Run' with: [ :editor |
					inspector go: editor text asString withUnixLineEndings. ] ].
	^inspector openOn: '# Examples:
#  breath_history(start=30, end=90)
#  callback_history(pattern="myapp")
#  breath_duration(plow=0.9, phigh=1)
#  callback_efficiency()
'.
]

{ #category : #'as yet unclassified' }
TimelineInspector >> buildExpr: script [
	"Assemble Nix expression to evaluate R code."
	| dir |
	dir := self summaryDir pathString.
	^ 'with import <studio>;
	timeliner.process({summaryData="', dir, '"; script="', script, '";})'.
]

{ #category : #'scripting opening' }
TimelineInspector >> go: code [
	[ :job |
		| tmp product |
		job title: 'Studio backend is evaluating R expressions'.
		tmp := FileReference newTempFilePrefix: 'eval-' suffix: '.R'.
		tmp writeStream nextPutAll: code; cr; close.
		product := [ NixBuild onCode: (self buildExpr: tmp pathString)  ]
			on: NixError do: [ :error |
				| dialog |
				dialog := NixErrorWindow openWithMessage: error messageText.
				dialog window
					position: self window position + (self window extent * 0.05);
					extent: self window extent * 0.9.
				^ nil ].
		job title: 'Studio frontend is analyzing data from the backend'.
		^ self inspectProduct: product.
	] asJob run.
]

{ #category : #'as yet unclassified' }
TimelineInspector >> gtInspectorRLibraryIn: composite [
	<gtInspectorPresentationOrder: 6>
	Halt now.
]

{ #category : #'as yet unclassified' }
TimelineInspector >> inspectProduct: aNixBuild [
	^ aNixBuild firstOutput entries first asFileReference.
]

{ #category : #setter }
TimelineInspector >> processDir: dir [ 
	processDir := dir.
]

{ #category : #'scripting opening' }
TimelineInspector >> summary [
	[ :job |
		| product |
		job title: 'Studio backend is building R data'.
		product := [ NixBuild onCode: (self summaryExpr)  ]
			on: NixError do: [ :error |
				| dialog |
				dialog := NixErrorWindow openWithMessage: error messageText.
				dialog window
					position: self window position + (self window extent * 0.05);
					extent: self window extent * 0.9.
				^ nil ].
		job title: 'Studio frontend is importing R summary'.
		^ product firstOutput.
	] asJob run.
]

{ #category : #setter }
TimelineInspector >> summaryDir [
	(summaryDir isNil) ifTrue: [ summaryDir := self summary ].
	^ summaryDir.
]

{ #category : #'as yet unclassified' }
TimelineInspector >> summaryExpr [
	"Assemble Nix expression to evaluate R code."
	^ 'with import <studio>;
	timeliner.summary("', (processDir pathString), '")'.
]
