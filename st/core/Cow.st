Object subclass: CowInstruction [
    |cow |

]

Object subclass: Cow [
    |result|

    out: aText [
        result := result + aText.
    ]
    
    outNewLine [
        self outString: Transcript getLineSeparator.
    ]
    
    outChar: code [
        out: (Character value: code)
    ]
    
    listenCareFuly: [
        ^ CowBrain new:self.
    ]
]

Object subclass: CowBrain [
    |memory cow|

    (+) new: aCow [
    	^ super new initialize:aCow.
    ]

    initialize: aCow [
        memory := Array new:10.	     
        cow := aCow. 
    ]
    
    moo [
        memory add:0.
    ]
    
    mOo [
        memory add:1.
    ]
    
    moO [
        memory add:2.
    ]
    
    mOO [
        memory add:3.
    ]
    
    Moo [
        memory add:4.
    ]
    
    MOo [
        memory add:5.
    ]   
    
    MoO [
        memory add 6.
    ]
    MOO [
        memory add 7.
    ]
        
    OOO [
        memory add 8.
    ]
        
    MMM [
        memory add 9.
    ]
    OOM [
        memory add 10.
    ]
        
    oom [
        memory add 11.
    ] 
    
    whatYouSay [
        cow do:self.
    ]
]

function exec(instruction) {
    switch (instruction) {
        // moo
        case 0:
            if (progPos == 0) {
                return false
            } else {
                progPos--  // skip previous command.
                level = 1
                while (level > 0) {
                    if (progPos == 0) {
                        break
                    }
                    progPos--
                    if (cowProgram[progPos] == 0) {
                        level++
                    } else if (cowProgram[progPos] == 7) {  // look for MOO
                        level--
                    }
                }
                if (level != 0) {
                    return false
                }
                return exec(cowProgram[progPos])
            }
            break
    
        // mOo
        case 1:
            if (memPos == 0) {
                return false
            } else {
                memPos--
            }
            break

        // moO
        case 2:
            memPos++
            if (memPos == memory.length) {
                memory.push(0)
            }
            break
    
        // mOO    
        case 3:
            if (memory[memPos] == 3) {
                return false
            } else {
                return exec(memory[memPos])
            }
            break
    
        // Moo
        case 4:
            if (memory[memPos] != 0) {
                printChar(memory[memPos])
            } else {
                memory[memPos] = Text.charCodeAt(window.prompt("please enter a character"))
            }
            break
        
        // MOo
        case 5:
            memory[memPos]--
            break
        
        // MoO
        case 6:
            memory[memPos]++
            break
        
        // MOO
        case 7:
            if (memory[memPos] == 0) {
                var level = 1
                var prev = 0
                progPos++  // have to skip past next command when looking for next moo.
                if (progPos == cowProgram.length) {
                    break
                }
                while (level > 0) {
                    prev = cowProgram[progPos]
                    progPos++
                    if (progPos == cowProgram.length) {
                        break
                    }
                    if (cowProgram[progPos] == 7) {
                        level++
                    } else {
                        if (cowProgram[progPos] == 0) {  // look for moo command.
                            level--
                            if (prev == 7) {
                                level--
                            }
                        }
                    }
                }
                if (level != 0) {
                    return false
                }
            }
            break
        
        // OOO
        case 8:
            memory[memPos] = 0
            break
        
        // MMM
        case 9:
            if (registerVal == undefined) {
                registerVal = memory[memPos]
            } else {
                memory[memPos] = registerVal
                registerVal = undefined
            }
            break
        
        // OOM
        case 10:
            printNumber(memory[memPos])
            break
        
        // oom
        case 11:
            memory[memPos] = parseFloat(window.prompt("please enter a number"))
            break
        
        // bad stuff
        default:
            return false
    }
    progPos++
    return true
}

function executeProgram() {
    // read program
    result = ""
    var tokens = [ "moo", "mOo", "moO", "mOO", "Moo", "MOo",
        "MoO", "MOO", "OOO", "MMM", "OOM", "oom"]
    cowIn = document.getElementById("CowProgram")
    var token = "   "
    cowProgram = new Array()
    for (var i = 0; i < cowIn.value.length; i++) {
        token = token.substring(1) + cowIn.value.charAt(i)
        for (var j = 0; j < tokens.length; j++) {
            if (tokens[j] == token) {
                cowProgram.push(j)
                token = "   "
                break
            }
        }
    }

    // execute
    progPos = 0
    registerVal = undefined
    memory = new Array()
    memPos = 0
    memory.push(0)
    while (progPos != cowProgram.length) {
        if (!exec(cowProgram[progPos])) {
            break
        }
    }
    newline()
    print("Done.")
    cowOut = document.getElementById("CowResult")
    cowOut.value = result
}

function generateProgram() {
    // initial program: store 8, 16, 32 and 64 in memory positions 1, 2, 3 and 4, current memory position is 5
    var program = "OOOMoOMoOMoOMoOMoOMoOMoOMoOMMMmoOMMMMMMmoOMMMMOOMOomOoMoOmoOmoomOoMMMmoOMMMMMMmoOMMMMOOMOomOoMoOmoOmoomOoMMMmoOMMMMMMmoOMMMMOOMOomOoMoOmoOmoo"

    cowOut = document.getElementById("CowResult")
    for (var i = 0; i < cowOut.value.length; i++) {
        // next character, ignore non-ASCII
        var c = cowOut.value.charCodeAt(i)
        if (c > 127) continue;
        
        // clear position 5 and 6
        program += "OOOmoOOOOmOo"
        
        // add bits
        for (var j = 6; j > 3; j--) {
            var mask = 1 << j
            if (c > mask) {
                switch (j) {
                    case 6:
                        // position 5 = 64
                        program += "mOoMMMmoOMMM"
                        break
                    case 5:
                        // position 5 = 32
                        program += "mOomOoMMMmoOmoOMMM"
                        break
                    case 4:
                        // position 5 = 16
                        program += "mOomOomOoMMMmoOmoOmoOMMM"
                        break
                }
                // add position 5 and 6, result in position 6
                program += "MOOMOomoOMoOmOomoo"
                c -= mask
            }
        }
        
        // add rest to position 6 and print
        program += "moO"
        for (var j = 0; j < c; j++) {
            program += "MoO"
        }
        program += "MoomOo"
    }

    // split in lines with max 66 characters
    var programFormatted = ""
    while (true) {
        if (program.length > 66) {
            programFormatted += program.substring(0, 66)
            programFormatted += "\n"
            program = program.substring(66)
        } else {
            programFormatted += program
            programFormatted += "\n"
            break
        }
    }
    cowIn = document.getElementById("CowProgram")
    cowIn.value = programFormatted
}

typedef std::vector<int> mem_t;
mem_t program;
mem_t memory;
mem_t::iterator mem_pos;
mem_t::iterator prog_pos;

int register_val;
bool has_register_val = false;

void quit( bool error )
{
    if( error )
    {
        printf( "\nERROR!\n" );
        exit(1);
    }
    
    printf( "\nDone.\n" );
    exit(0);
}

bool exec( int instruction )
{
// printf( "EXEC: %d\n", instruction );

    switch( instruction )
    {
    // moo
    case 0:
        {
            if( prog_pos == program.begin() )
                quit( true );

            prog_pos--; // skip previous command.
            int level = 1;
            while( level > 0 )
            {
                if( prog_pos == program.begin() )
                    break;

                prog_pos--;
            
                if( (*prog_pos) == 0 )
                    level++;
                else
                if( (*prog_pos) == 7 ) // look for MOO
                    level--;
            }

            if( level != 0 )
                quit(true);

            return exec( *prog_pos );
        }
    
    // mOo
    case 1:
        if( mem_pos == memory.begin() )
            quit( true );
        else
            mem_pos--;
        break;

    // moO
    case 2:
        mem_pos++;
        if( mem_pos == memory.end() )
        {
            memory.push_back(0);
            mem_pos = memory.end();
            mem_pos--;
        }
        break;
    
    // mOO
    case 3:
        if( (*mem_pos) == 3 )
            quit( false );
        return exec(*mem_pos);
    
    // Moo
    case 4:
        if( (*mem_pos) != 0 )
            printf( "%c", *mem_pos );
        else
        {
            (*mem_pos) = getchar();
            while( getchar() != '\n' );
        }
        break;
    
    // MOo
    case 5:
        (*mem_pos)--;
        break;
    
    // MoO
    case 6:
        (*mem_pos)++;
        break;

    // MOO
    case 7:
        if( (*mem_pos) == 0 )
        {
            int level = 1;
            int prev = 0;
            prog_pos++; // have to skip past next command when looking for next moo.
            if( prog_pos == program.end() )
                break;
            while( level > 0 )
            {
                prev = *prog_pos;
                prog_pos++;
                
                if( prog_pos == program.end() )
                    break;
                
                if( (*prog_pos) == 7 )
                    level++;
                else
                if( (*prog_pos) == 0 )  // look for moo command.
                {
                    level--;
                    if( prev == 7 )
                        level--;
                }
            }
            if( level != 0 )
                quit( true );
        }
        break;
    
    // OOO
    case 8:
        (*mem_pos) = 0;
        break;

    // MMM
    case 9:
        if( has_register_val )
            (*mem_pos) = register_val;
        else
            register_val = (*mem_pos);
        has_register_val = !has_register_val;
        break;

    // OOM
    case 10:
        printf( "%d\n", *mem_pos );
        break;
    
    // oom
    case 11:
        {
            char buf[100];
            int c = 0;
            while( c < sizeof(buf)-1 )
            {
                buf[c] = getchar();
                c++;
                buf[c] = 0;
                
                if( buf[c-1] == '\n' )
                    break;
            }
            // swallow, just in case.
            if( c == sizeof(buf) )
                while( getchar() != '\n' );
            
            (*mem_pos) = atoi( buf );

            break;
        }

    // bad stuff
    default:
        quit( false );
    };

    prog_pos++;

    return true;
}


int main( int argc, char** argv )
{
if( argc < 2 )
{
printf( "Usage: %s program.cow\n\n", argv[0] );
exit( 1 );
}

FILE* f = fopen( argv[1], "rb" );

if( f == NULL )
{
printf( "Cannot open source file [%s].\n", argv[1] );
        exit( 1 );
}

    char buf[3];
    memset( buf, 0, 3 );
    int pos = 0;

    while( !feof(f) )
    {
        int found = 0;
        buf[2] = fgetc( f );

        if( found = !strncmp( "moo", buf, 3 ) )
            program.push_back( 0 );
        else if( found = !strncmp( "mOo", buf, 3 ) )
            program.push_back( 1 );
        else if( found = !strncmp( "moO", buf, 3 ) )
            program.push_back( 2 );
        else if( found = !strncmp( "mOO", buf, 3 ) )
            program.push_back( 3 );
        else if( found = !strncmp( "Moo", buf, 3 ) )
            program.push_back( 4 );
        else if( found = !strncmp( "MOo", buf, 3 ) )
            program.push_back( 5 );
        else if( found = !strncmp( "MoO", buf, 3 ) )
            program.push_back( 6 );
        else if( found = !strncmp( "MOO", buf, 3 ) )
            program.push_back( 7 );
        else if( found = !strncmp( "OOO", buf, 3 ) )
            program.push_back( 8 );
        else if( found = !strncmp( "MMM", buf, 3 ) )
            program.push_back( 9 );
        else if( found = !strncmp( "OOM", buf, 3 ) )
            program.push_back( 10 );
        else if( found = !strncmp( "oom", buf, 3 ) )
            program.push_back( 11 );
            
        if( found )
        {
            memset( buf, 0, 3 );
        }
        else
        {
            buf[0] = buf[1];
            buf[1] = buf[2];
            buf[2] = 0;
        }
    }

fclose( f );

printf( "Welcome to COW!\n\nExecuting [%s]...\n\n", argv[1] );

    // init main memory.
    memory.push_back( 0 );
    mem_pos = memory.begin();

    prog_pos = program.begin();
    while( prog_pos != program.end() )
        if( !exec( *prog_pos ) )
            break;

    quit( false );

return 0;
}

