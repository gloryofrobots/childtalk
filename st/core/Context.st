Context extend [
    
    showTraceBack [
        self tracebackString echo.
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
        <primitive: 'ContextPart_receiver'>
        self primitiveFailed
    ]
    
    parent [
        <primitive: 'ContextPart_parent'>
        self primitiveFailed
    ]
]