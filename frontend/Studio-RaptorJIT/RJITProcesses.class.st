Class {
	#name : #RJITProcesses,
	#superclass : #Object,
	#instVars : [
		'processes',
		'path'
	],
	#category : #'Studio-RaptorJIT'
}

{ #category : #'instance creation' }
RJITProcesses >> fromPath: aPath [
	path := aPath.
	processes := OrderedCollection new.
	[ processes add: (RJITProcess new fromPath: aPath) ]
		ifError: [ "Root is not a RJITProcess" ].
	aPath children do:
		[ :child |
			[ processes add: (RJITProcess new fromPath: child) ]
				ifError: [  "Child directory is not a RJITProcess" ] ].
	processes size = 1 ifTrue: [ ^ processes first ].
]

{ #category : #'as yet unclassified' }
RJITProcesses >> gtInspectorFilesIn: composite [
	<gtInspectorPresentationOrder: 10>
	^ path gtInspectorItemsIn: composite.
]

{ #category : #'as yet unclassified' }
RJITProcesses >> gtInspectorProcessesIn: composite [
	<gtInspectorPresentationOrder: 5>
	^ composite fastTable
		title: 'RaptorJIT processes';
		display: #processes;
		column: 'Id' evaluated: [ :process | process path basename ] width: 100;
		column: 'Profiles' evaluated: [ :process | process vmprofiles size ] width: 60;
		column: 'Samples' evaluated: #totalSamples width: 60.
		
		
]

{ #category : #acccessing }
RJITProcesses >> processes [
	^ processes
]
