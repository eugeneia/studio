Class {
	#name : #RJITTraceFlushAllEvent,
	#superclass : #RJITEvent,
	#instVars : [
		'event'
	],
	#category : #'Studio-RaptorJIT'
}

{ #category : #'instance creation' }
RJITTraceFlushAllEvent >> from: aDict flashback: aFlashback [
	event := aDict at: 'event'.
	flashback := aFlashback. 

]

{ #category : #testing }
RJITTraceFlushAllEvent >> isTraceFlushEvent [
	^ true
]

{ #category : #printing }
RJITTraceFlushAllEvent >> printOn: aStream [
	aStream
		nextPutAll: '<event: ';
		nextPutAll: event;
		nextPutAll: ' (previous traces are discarded and related events omitted)';
		nextPutAll: '>'.
]
