Object subclass: ArgTest [
    sum: arg1 with: arg2 with: arg3 [
        ^ (arg1 + arg2 + arg3).
    ]
]
    
Eval [
    |x|
   x := ArgTest new sum:2 with:3 with: 4.
   x print.
]