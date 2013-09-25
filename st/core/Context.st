Context extend [
    
    primitiveFailed [
        Signal fatal:'Context primitive error'
    ]
    
    showTraceBack [
        self tracebackString echo.
    ]
    
    process [
        <primitive: 'Context_process'>
        self primitiveFailed
    ]
    
    die [
        <primitive: 'Context_die'>
        self primitiveFailed
    ]
    
    parent [
        <primitive: 'Context_parent'>
        self primitiveFailed
    ]
    
    handle: aSignal [
        <primitive: 'Context_handle'>
        self primitiveFailed
    ]
    
    signalHandler [
        <primitive: 'Context_signalHandler'>
        self primitiveFailed
    ]
    
    handledSignal [
        <primitive: 'Context_handledSignal'>
        self primitiveFailed
    ]
    
    handles:aSignal [
        ^ self handledSignal = aSignal class
    ]
    
    tracebackString [
        <primitive: 'Context_traceback'>
        self primitiveFailed
    ]
    
    echo [
        <primitive: 'Context_echo'>
    ]
    
    method [
        <primitive: 'Context_method'>
        self primitiveFailed
    ]
    
    receiver [
        <primitive: 'Context_receiver'>
        self primitiveFailed
    ]
]