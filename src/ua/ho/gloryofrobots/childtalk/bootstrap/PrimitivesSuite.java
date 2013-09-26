package ua.ho.gloryofrobots.childtalk.bootstrap;

import java.util.Calendar;
import java.util.TimeZone;

import ua.ho.gloryofrobots.childtalk.Universe;
import ua.ho.gloryofrobots.childtalk.inout.InOutSuite;
import ua.ho.gloryofrobots.childtalk.inout.SignalSuite;
import ua.ho.gloryofrobots.childtalk.scheduler.Routine;
import ua.ho.gloryofrobots.childtalk.scheduler.SchedulingSuite;
import ua.ho.gloryofrobots.childtalk.stobject.STArray;
import ua.ho.gloryofrobots.childtalk.stobject.STBlock;
import ua.ho.gloryofrobots.childtalk.stobject.STByteObject;
import ua.ho.gloryofrobots.childtalk.stobject.STCharacter;
import ua.ho.gloryofrobots.childtalk.stobject.STClass;
import ua.ho.gloryofrobots.childtalk.stobject.STCollection;
import ua.ho.gloryofrobots.childtalk.stobject.STContext;
import ua.ho.gloryofrobots.childtalk.stobject.STExecutableObject;
import ua.ho.gloryofrobots.childtalk.stobject.STFloating;
import ua.ho.gloryofrobots.childtalk.stobject.STInternalDictionary;
import ua.ho.gloryofrobots.childtalk.stobject.STLargeInteger;
import ua.ho.gloryofrobots.childtalk.stobject.STMetaclass;
import ua.ho.gloryofrobots.childtalk.stobject.STNumber;
import ua.ho.gloryofrobots.childtalk.stobject.STObject;
import ua.ho.gloryofrobots.childtalk.stobject.STPrimitive;
import ua.ho.gloryofrobots.childtalk.stobject.STProcess;
import ua.ho.gloryofrobots.childtalk.stobject.STSmallInteger;
import ua.ho.gloryofrobots.childtalk.stobject.STStack;
import ua.ho.gloryofrobots.childtalk.stobject.STString;
import ua.ho.gloryofrobots.childtalk.stobject.STSymbol;

@SuppressWarnings("serial")
public class PrimitivesSuite {
    private static class NumberPrimitives {

        public STPrimitive add = new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STNumber first = receiver.castToSubclass();
                STObject arg = routine.getArgument(0);
                STNumber second = getStrictCastedObject(routine,
                        arg, "Number");

                return STNumber.add(first, second);
            }
        };

        public STPrimitive substract = new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STNumber first = receiver.castToSubclass();

                STNumber second = getStrictCastedObject(routine,
                        routine.getArgument(0), "Number");

                return STNumber.substract(first, second);
            }
        };

        public STPrimitive multiply = new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STNumber first = receiver.castToSubclass();

                STNumber second = getStrictCastedObject(routine,
                        routine.getArgument(0), "Number");

                return STNumber.multiply(first, second);
            }
        };

        public STPrimitive divide = new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STNumber first = receiver.castToSubclass();

                STNumber second = getStrictCastedObject(routine,
                        routine.getArgument(0), "Number");

                return STNumber.divide(first, second);
            }
        };

        public final STPrimitive lessThen = new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STNumber first = receiver.castToSubclass();

                STNumber second = getStrictCastedObject(routine,
                        routine.getArgument(0), "Number");

                return STNumber.lessThen(first, second);
            }
        };

        public STPrimitive greaterThen = new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STNumber first = receiver.castToSubclass();

                STNumber second = getStrictCastedObject(routine,
                        routine.getArgument(0), "Number");

                return STNumber.greaterThen(first, second);
            }
        };

        public STPrimitive lessEqual = new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STNumber first = receiver.castToSubclass();

                STNumber second = getStrictCastedObject(routine,
                        routine.getArgument(0), "Number");
                // DebugSuite.printTraceBackString(routine);

                return STNumber.lessEqual(first, second);
            }
        };

        public STPrimitive greaterEqual = new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STNumber first = receiver.castToSubclass();

                STNumber second = getStrictCastedObject(routine,
                        routine.getArgument(0), "Number");

                return STNumber.greaterEqual(first, second);
            }
        };

        public STPrimitive equal = new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STNumber first = receiver.castToSubclass();

                STNumber second = getStrictCastedObject(routine,
                        routine.getArgument(0), "Number");

                return STNumber.equal(first, second);
            }
        };

        public STPrimitive notEqual = new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STNumber first = receiver.castToSubclass();

                STNumber second = getStrictCastedObject(routine,
                        routine.getArgument(0), "Number");

                return STNumber.notEqual(first, second);
            }
        };

        public STPrimitive mod = new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STNumber first = receiver.castToSubclass();

                STNumber second = getStrictCastedObject(routine,
                        routine.getArgument(0), "Number");

                return STNumber.mod(first, second);
            }
        };
        
        public STPrimitive toSTString = new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STNumber first = receiver.castToSubclass();
                return first.toSTString();
            }
        };
    }

    public static <T extends STObject> T getStrictCastedObject(Routine routine,
            STObject obj, String typeName) {
        try {
            T result = obj.castToSubclass();
            if (result == null) {
                SignalSuite.raiseError(routine,
                        "Expected type : %s but got instance of %s", typeName,
                        obj.getSTClass().getName());
            }

            return result;
        } catch (ClassCastException e) {
            SignalSuite.raiseError(routine,
                    "Expected type : %s but got instance of %s", typeName, obj
                            .getSTClass().getName());
            return null;
        }
    }

    public static void initialisePrimitives() {
        initialiseBehaviour();
        initialiseBlock();
        initialiseObject();
        initialiseByteArray();
        
        initialiseNumber();
        initialiseSmallInteger();
        initialiseLargeInteger();
        initialiseFloat();
        initialiseCharacter();
        initialiseString();

        initialiseArray();
        initialiseCollection();
        initialiseDateTime();
        initialiseContext();

        initialiseSignal();
        initialiseInternalDictionary();
        initialiseTranscript();
        initialiseSystem();
        initialiseProcess();
        
    }

    private static void initialiseProcess() {
        STClass process = Universe.classes().Process;
        process.setPrimitive("Process_resume", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STProcess process = receiver.castToSubclass();
                if(process.isTerminated() == true) {
                    return Universe.objects().FALSE;
                }
                
                process.activate();
                return Universe.objects().TRUE;
            }
        });
        process.setPrimitive("Process_suspend", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STProcess process = receiver.castToSubclass();
                if(process.isTerminated() == true) {
                    return Universe.objects().FALSE;
                }
                
                process.suspend();
                return Universe.objects().TRUE;
            }
        });
    }

    private static void initialiseInternalDictionary() {
        STClass dict = Universe.classes().InternalDictionary;
        dict.setPrimitive("InternalDictionary_at_put", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STObject key = routine.getArgument(0);
                STObject val = routine.getArgument(1);
                STInternalDictionary dict = receiver.castToSubclass();
                dict.put(key, val);
                
                return Universe.objects().NIL;
            }
        });
        
        dict.setPrimitive("InternalDictionary_at", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STObject key = routine.getArgument(0);
                
                STInternalDictionary dict = receiver.castToSubclass();
                STObject result = dict.at(key);
                
                if(result == null) {
                    return Universe.objects().NIL;
                }
                
                return result;
            }
        });
        
    }

    private static void initialiseSystem() {
        STClass system = Universe.classes().System;
        system.setPrimitive("System_getEnv", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STObject arg = routine.getArgument(0);
                String argStr = arg.toString();
                String var = System.getenv(argStr);
                return STString.create(var);
            }
        });
        system.setPrimitive("System_getProperty", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STObject arg = routine.getArgument(0);
                String argStr = arg.toString();
                String var = System.getProperty(argStr);
                return STString.create(var);
            }
        });
        
    }

    private static void initialiseNumber() {
        STClass number = Universe.classes().Number;
        number.setPrimitive("Number_toString", sNumberPrimitives.toSTString);
    }

    private static void initialiseTranscript() {
        // TODO Auto-generated method stub
        
        STClass transcript = Universe.classes().Transcript;
        transcript.setPrimitive("Transcript_show", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STByteObject obj = routine.getArgument(0).castToSubclass();
                InOutSuite.toStdOut(obj);
                
                return Universe.objects().NIL;
            }
        });
    }

    private static void initialiseSignal() {
        STClass signal = Universe.classes().Signal;
        signal.setPrimitive("Signal_raise", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                routine.raise(receiver);
                return Universe.objects().NIL;
            }
        });
        
        signal.setPrimitive("Signal_fatal", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STObject obj = routine.getArgument(0);
                SignalSuite.error(obj.toString());

                return Universe.objects().NIL;
            }
        });
    }

    private static void initialiseContext() {
        STClass context = Universe.classes().Context;
        context.setPrimitive("Context_echo", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STContext context = receiver.castToSubclass();
                Routine contextRoutine = context.getRouitne();
                if (contextRoutine == null) {
                    System.out.println("Empty context!");
                    return Universe.objects().NIL;
                }

                System.out.println(contextRoutine.toString());
                return Universe.objects().NIL;
            }
        });

        context.setPrimitive("Context_traceback", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STContext context = receiver.castToSubclass();
                Routine contextRoutine = context.getRouitne();
                if (contextRoutine == null) {
                    System.out.println("Empty context!");
                    return Universe.objects().NIL;
                }

                STString traceBackString = STString.create(DebugSuite
                        .getTraceBackString(contextRoutine));
                return traceBackString;
            }
        });

        context.setPrimitive("Context_parent", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STContext context = receiver.castToSubclass();
                return context.getParentContext();
            }
        });

        context.setPrimitive("Context_signalHandler", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STContext context = receiver.castToSubclass();
                STObject obj =  context.getSignalHandler();
                
                if(obj == null) {
                    return Universe.objects().NIL;
                }
                
                return obj;
            }
        });

        context.setPrimitive("Context_handledSignal", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STContext context = receiver.castToSubclass();
                STObject obj =  context.getHandledSignal();
                if(obj == null) {
                    return Universe.objects().NIL;
                }
                
                return obj;
            }
        });

        context.setPrimitive("Context_method", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STContext context = receiver.castToSubclass();
                return context.getExecutable();
            }
        });

        context.setPrimitive("Context_receiver", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STContext context = receiver.castToSubclass();
                return context.getReceiver();
            }
        });

        context.setPrimitive("Context_countArguments", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STContext context = receiver.castToSubclass();
                int count = context.getCountArguments();

                return STSmallInteger.create(count);
            }
        });
        context.setPrimitive("Context_handle", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {

                STContext context = receiver.castToSubclass();
                Routine contextRoutine = context.getRouitne();
                STObject signal = routine.getArgument(0);
                contextRoutine.handleSignal(signal);

                return Universe.objects().NIL;
            }
        });

        context.setPrimitive("Context_process", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STContext context = receiver.castToSubclass();
                return context.getProcess();
            }
        });

        context.setPrimitive("Context_die", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STContext context = receiver.castToSubclass();
                Routine contextRoutine = context.getRouitne();
                STProcess process = context.getProcess();
                process.terminateFromRoutine(contextRoutine);
                return Universe.objects().NIL;
            }
        });
    }

    public static int getJavaColectionIndexFromSmalltalk(STSmallInteger index) {
        return index.toInt() - 1;
    }

    private static void initialiseCollection() {
        // TODO Auto-generated method stub
        STClass collection = Universe.classes().Collection;
        collection.setPrimitive("Collection_size", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STCollection collection = receiver.castToSubclass();
                return STSmallInteger.create(collection.size());
            }
        });
        
        collection.setPrimitive("Collection_newColon", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STSmallInteger sizeArg = routine.getArgument(0).castToSubclass();
                int size = sizeArg.toInt();
                STClass klass = receiver.castToSubclass();
                
                STCollection collection = STCollection.create(size, klass);
                return collection;
            }
        });
        
        collection.setPrimitive("Collection_growTo", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STCollection collection = receiver.castToSubclass();
                
                STSmallInteger arg = routine.getArgument(0).castToSubclass();

                if (arg == null) {
                    primitiveError(routine, "Invalid Collection index %s",
                            routine.getArgument(0).toString());

                    return null;
                }

                int size = arg.toInt();
                if (size <= collection.size() || size < 0) {
                    primitiveError(routine, "Invalid Collection grow index %d ",
                            size);

                    return null;
                }
                
                collection.growTo(size);
                return Universe.objects().NIL;
            }
        });
        
        collection.setPrimitive("Collection_at", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STSmallInteger indexArgument = (STSmallInteger) routine
                        .getArgument(0);

                STCollection collection = receiver.castToSubclass();

                if (indexArgument == null) {
                    primitiveError(routine, "Invalid Collection index %s",
                            routine.getArgument(0).toString());

                    return null;
                }

                int index = getJavaColectionIndexFromSmalltalk(indexArgument);
                if (index >= collection.size() || index < 0) {
                    primitiveError(routine, "Invalid Collection index %d ",
                            index);

                    return null;
                }

                STObject value = collection.at(index);
                if (value == null) {
                    primitiveError(routine, "Invalid Collection index %d ",
                            index);

                    return null;
                }
                return value;
            }
        });

        collection.setPrimitive("Collection_at_put", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STSmallInteger indexArgument = routine.getArgument(0)
                        .castToSubclass();
                STObject valueArgument = routine.getArgument(1);

                if (indexArgument == null) {
                    primitiveError(routine, "Invalid Collection put index %s",
                            routine.getArgument(0).toString());

                    return null;
                }

                if (valueArgument == null) {
                    primitiveError(routine, "Invalid Collection put value %s",
                            routine.getArgument(1).toString());

                    return null;
                }

                STCollection collection = receiver.castToSubclass();

                int index = getJavaColectionIndexFromSmalltalk(indexArgument);
                if (index > collection.size() || index < 0) {
                    primitiveError(routine, "Invalid Collection put index %d ",
                            index);

                    return null;
                }

                collection.put(index, valueArgument);
                return Universe.objects().NIL;
            }
        });
    }

    private static void initialiseArray() {
        STClass array = Universe.classes().Array;
        array.setPrimitive("Array_newColon", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                
                STSmallInteger sizeArg = routine.getArgument(0).castToSubclass();
                
                int size = sizeArg.toInt();                
                STArray array = STArray.create(size);
                return array;
            }
        });
        
        array.setPrimitive("Array_add", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                
                STObject obj = routine.getArgument(0);
                
                STArray array = receiver.castToSubclass();
                
                array.add(obj);
                
                return Universe.objects().NIL;
            }
        });
        
    }


    private static void initialiseDateTime() {
        STClass dateTime = Universe.classes().DateTime;
        dateTime.setPrimitive("DateTime_gmTime", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                Calendar calendar = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
                
                STArray data = STArray.create(8);
                
                data.put(0, STSmallInteger.create(calendar.get(Calendar.SECOND)));
                data.put(1, STSmallInteger.create(calendar.get(Calendar.MINUTE)));
                data.put(2, STSmallInteger.create(calendar.get(Calendar.HOUR)));
                data.put(3, STSmallInteger.create(calendar.get(Calendar.DAY_OF_MONTH)));
                data.put(4, STSmallInteger.create(calendar.get(Calendar.MONTH)));
                data.put(5, STSmallInteger.create(calendar.get(Calendar.YEAR)));
                data.put(6, STSmallInteger.create(calendar.get(Calendar.DAY_OF_WEEK)));
                data.put(7, STSmallInteger.create(calendar.get(Calendar.DAY_OF_YEAR)));
                
                return  data;
            }
        });
    }

    private static void initialiseString() {
        STClass string = Universe.classes().String;
        string.setPrimitive("String_asSymbol", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STString str = receiver.castToSubclass();
                return STSymbol.create(str.toString());
            }
        });

        string.setPrimitive("String_hash", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STByteObject string = receiver.castToSubclass();
                long hash = string.getHash();
                return STLargeInteger.create(hash);
            }
        });
    }

    private static void initialiseObject() {
        STClass object = Universe.classes().Object;
        object.setPrimitive("Object_class", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STClass klass = receiver.getSTClass();
                return klass;
            }
        });

        object.setPrimitive("Object_identityEqual", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STObject argument = routine.getArgument(0);

                if (argument == receiver) {
                    return Universe.objects().TRUE;
                }

                return Universe.objects().FALSE;
            }
        });

        object.setPrimitive("Object_equal", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STObject argument = routine.getArgument(0);

                if (argument.equals(receiver) == true) {
                    return Universe.objects().TRUE;
                }

                return Universe.objects().FALSE;
            }
        });

        object.setPrimitive("Object_hash", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {

                return STSmallInteger.create(receiver.hashCode());
            }
        });

        object.setPrimitive("Object_copy", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                return receiver.shallowCopy();
            }
        });

        object.setPrimitive("Object_perform", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                
                STObject selector = routine.getArgument(0).toSymbol();
                //flush all arguments except first=selector.
                routine.flushArgumentsToStack(stack, 1, routine.getCountArguments());
                SchedulingSuite.callForSelector(routine, receiver.getSTClass(), selector);

                return Universe.objects().NIL;
            }
        });
        
        object.setPrimitive("Object_performWithArguments", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STObject selector = routine.getArgument(0).toSymbol();
                STObject args = routine.getArgument(1);
                STCollection collection = getStrictCastedObject(routine, args, "Collection");
                stack.flushCollection(collection);
                
                SchedulingSuite.callForSelector(routine, receiver.getSTClass(), selector);
                return Universe.objects().NIL;
            }
        });
        
        object.setPrimitive("Object_println", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                // TODO DELETE
                InOutSuite.toStdOut(receiver.toString());
                return Universe.objects().NIL;
            }
        });

    }

    public static void initialiseBlock() {
        STClass block = Universe.classes().Block;
        block.setPrimitive("Block_execute", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                SchedulingSuite.callExecutable(routine,
                        (STExecutableObject) receiver);
                return Universe.objects().NIL;
            }
        });

        block.setPrimitive("Block_on_do_ensure", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STObject exception = routine.getArgument(0);
                STBlock block = routine.getArgument(1).castToSubclass();
                STExecutableObject ensuredBlock;
                STObject ensured = routine.getArgument(2);
                
                if(ensured == Universe.objects().NIL){
                    ensuredBlock = null;
                } else {
                    ensuredBlock = (STExecutableObject) ensured;
                }
                
                SchedulingSuite.callExecutableWithExceptionHandling(routine,
                        (STExecutableObject) receiver, exception, block, ensuredBlock);

                return Universe.objects().NIL;
            }
        });

        block.setPrimitive("Block_newProcess", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                
                STBlock block = (STBlock) receiver;
                STProcess process = SchedulingSuite.callExecutableInNewProcess(block);
                process.suspend();
                return process;
            }
        });

        block.setPrimitive("Block_value", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STBlock block = (STBlock) receiver;
                //stack.push(block);
                routine.flushArgumentsToStack(stack);
                SchedulingSuite.callExecutable(routine, block);
                return Universe.objects().NIL;
            }
        });
    }

    private static void initialiseBehaviour() {
        STClass behaviour = Universe.classes().Behaviour;
        behaviour.setPrimitive("Behavior_new", new STPrimitive() {

            @SuppressWarnings("unchecked")
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {

                STObject object = null;

                STClass klass = (STClass) receiver;
                Class internalClass = klass.getInternalObjectClass();
                if (internalClass != null) {
                    object = STObject.newObject(klass, internalClass);
                } else {
                    object = STObject.createWithClass(klass);
                }
                if (object == null) {
                    SignalSuite.error("Can`t create object of class %s",
                            klass.toString());
                }

                return object;
            }
        });

        behaviour.setPrimitive("Behaviour_createSubclass", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STMetaclass
                        .createClassInRuntime(this, routine, receiver, stack);
                return Universe.objects().NIL;
            }
        });

        behaviour.setPrimitive("Behavior_doesUnderstand", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STObject key = routine.getArgument(0);
                STClass klass = receiver.castToSubclass();

                if (klass.findMethod(key) != null) {
                    return Universe.objects().TRUE;
                }

                return Universe.objects().FALSE;
            }
        });
    }

    private static void initialiseByteArray() {
        STClass byteArray = Universe.classes().ByteArray;

        byteArray.setPrimitive("ByteArray_newColon", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STSmallInteger argument = (STSmallInteger) routine
                        .getArgument(0);
                STByteObject obj = STByteObject.create(argument.toInt());
                obj.setSTClass((STClass) receiver);
                return obj;
            }
        });

        byteArray.setPrimitive("ByteArray_growTo", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STSmallInteger argument = (STSmallInteger) routine
                        .getArgument(0);

                STByteObject obj = receiver.castToSubclass();
                obj.growTo(argument.toInt());

                return Universe.objects().NIL;
            }
        });

        byteArray.setPrimitive("ByteArray_size", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STByteObject obj = receiver.castToSubclass();
                return STSmallInteger.create(obj.size());
            }
        });

        byteArray.setPrimitive("ByteArray_at", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STSmallInteger indexArgument = (STSmallInteger) routine
                        .getArgument(0);

                STByteObject obj = receiver.castToSubclass();

                if (indexArgument == null) {
                    primitiveError(routine, "Invalid ByteArray index %s",
                            routine.getArgument(0).toString());

                    return null;
                }

                int index = getJavaColectionIndexFromSmalltalk(indexArgument);
                if (index > obj.size() || index < 0) {
                    primitiveError(routine, "Invalid ByteArray index %d ",
                            index);

                    return null;
                }

                byte value = obj.at(index);

                return STSmallInteger.create((int) value);
            }
        });

        byteArray.setPrimitive("ByteArray_at_put", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STSmallInteger indexArgument = routine.getArgument(0)
                        .castToSubclass();
                STSmallInteger valueArgument = routine.getArgument(1)
                        .castToSubclass();

                if (indexArgument == null) {
                    primitiveError(routine, "Invalid ByteArray index %s",
                            routine.getArgument(0).toString());

                    return null;
                }

                if (valueArgument == null) {
                    primitiveError(routine, "Invalid ByteArray value %s",
                            routine.getArgument(1).toString());

                    return null;
                }

                STByteObject obj = receiver.castToSubclass();

                int index = getJavaColectionIndexFromSmalltalk(indexArgument);
                if (index > obj.size() || index < 0) {
                    primitiveError(routine, "Invalid ByteArray index %d ",
                            index);

                    return null;
                }

                int value = valueArgument.toInt();
                if (value < 0 || value > 255) {
                    primitiveError(routine, "Expected byte value, but got %d ",
                            value);

                    return null;
                }
                obj.put(index, (byte) value);

                return Universe.objects().NIL;
            }
        });
    }

    private static void initialiseCharacter() {
        STClass character = Universe.classes().Character;
        character.setPrimitive("Character_new", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STSmallInteger value = (STSmallInteger) routine.getArgument(0);
                if (value == null) {
                    primitiveError(
                            routine,
                            "Invalid Character data, expected SmallInteger, but got %s",
                            routine.getArgument(0).toString());

                    return null;
                }

                return STCharacter.create((char) value.toInt());
            }
        });

        character.setPrimitive("Character_value", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STCharacter character = receiver.castToSubclass();
                int value = (int) character.toChar();
                return STSmallInteger.create(value);
            }
        });
    }

    private static void initialiseFloat() {

        STClass floating = Universe.classes().Float;

        floating.setPrimitive("Float_plus", sNumberPrimitives.add);
        floating.setPrimitive("Float_minus", sNumberPrimitives.substract);
        floating.setPrimitive("Float_mul", sNumberPrimitives.multiply);
        floating.setPrimitive("Float_div", sNumberPrimitives.divide);
        floating.setPrimitive("Float_lt", sNumberPrimitives.lessThen);
        floating.setPrimitive("Float_gt", sNumberPrimitives.greaterThen);
        floating.setPrimitive("Float_le", sNumberPrimitives.lessEqual);
        floating.setPrimitive("Float_ge", sNumberPrimitives.greaterEqual);
        floating.setPrimitive("Float_eq", sNumberPrimitives.equal);
        floating.setPrimitive("Float_ne", sNumberPrimitives.notEqual);
        floating.setPrimitive("Float_mod", sNumberPrimitives.mod);
        floating.setPrimitive("Float_ceil", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STFloating first = receiver.castToSubclass();
                return first.ceil();
            }
        });

        floating.setPrimitive("Float_floor", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STFloating first = receiver.castToSubclass();
                return first.floor();
            }
        });

        floating.setPrimitive("Float_trunc", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STFloating first = receiver.castToSubclass();
                return first.truncate();
            }
        });

        floating.setPrimitive("Float_newColon", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STNumber number = getStrictCastedObject(routine,
                        routine.getArgument(0), "Number");
                STNumber result = number
                        .convert(STNumber.FLOAT_INTEGER_PRIORITY);
                if (result == null) {
                    primitiveError(routine,
                            "Can`t create Float instance from %s ",
                            number.toString());
                }

                return result;
            }
        });

    }

    private static void initialiseLargeInteger() {
        // TODO Auto-generated method stub
        STClass largeInteger = Universe.classes().LargeInteger;

        largeInteger.setPrimitive("LargeInteger_plus", sNumberPrimitives.add);
        largeInteger.setPrimitive("LargeInteger_minus",
                sNumberPrimitives.substract);
        largeInteger.setPrimitive("LargeInteger_mul",
                sNumberPrimitives.multiply);
        largeInteger.setPrimitive("LargeInteger_div", sNumberPrimitives.divide);
        largeInteger
                .setPrimitive("LargeInteger_lt", sNumberPrimitives.lessThen);
        largeInteger.setPrimitive("LargeInteger_gt",
                sNumberPrimitives.greaterThen);
        largeInteger.setPrimitive("LargeInteger_le",
                sNumberPrimitives.lessEqual);
        largeInteger.setPrimitive("LargeInteger_ge",
                sNumberPrimitives.greaterEqual);
        largeInteger.setPrimitive("LargeInteger_eq", sNumberPrimitives.equal);
        largeInteger
                .setPrimitive("LargeInteger_ne", sNumberPrimitives.notEqual);
        largeInteger.setPrimitive("LargeInteger_mod", sNumberPrimitives.mod);

        largeInteger.setPrimitive("LargeInteger_bitAnd", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STLargeInteger first = receiver.castToSubclass();

                STSmallInteger second = getStrictCastedObject(routine,
                        routine.getArgument(0), "LargeInteger");

                return first.bitAnd(second.toInt());
            }
        });

        largeInteger.setPrimitive("LargeInteger_bitOr", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STLargeInteger first = receiver.castToSubclass();

                STSmallInteger second = getStrictCastedObject(routine,
                        routine.getArgument(0), "LargeInteger");

                return first.bitOr(second.toInt());
            }
        });

        largeInteger.setPrimitive("LargeInteger_bitXor", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STLargeInteger first = receiver.castToSubclass();

                STSmallInteger second = getStrictCastedObject(routine,
                        routine.getArgument(0), "LargeInteger");

                return first.bitXor(second.toInt());
            }
        });

        largeInteger.setPrimitive("LargeInteger_bitShift", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STLargeInteger first = receiver.castToSubclass();

                STSmallInteger second = getStrictCastedObject(routine,
                        routine.getArgument(0), "LargeInteger");

                return first.bitShift(second.toInt());
            }
        });

        largeInteger.setPrimitive("LargeInteger_asFloat", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STLargeInteger first = receiver.castToSubclass();

                return first.asFloat();
            }
        });

        largeInteger.setPrimitive("LargeInteger_clear", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STLargeInteger first = receiver.castToSubclass();

                first.clear();
                return Universe.objects().NIL;
            }
        });

        largeInteger.setPrimitive("LargeInteger_newColon", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STNumber number = getStrictCastedObject(routine,
                        routine.getArgument(0), "Number");
                STNumber result = number
                        .convert(STNumber.LARGE_INTEGER_PRIORITY);
                if (result == null) {
                    primitiveError(routine,
                            "Can`t create LargeInteger instance from %s ",
                            number.toString());
                }

                return result;
            }
        });
    }

    private static NumberPrimitives sNumberPrimitives = new NumberPrimitives();

    private static void initialiseSmallInteger() {

        STClass smallInteger = Universe.classes().SmallInteger;

        smallInteger.setPrimitive("SmallInteger_plus", sNumberPrimitives.add);
        smallInteger.setPrimitive("SmallInteger_minus",
                sNumberPrimitives.substract);
        smallInteger.setPrimitive("SmallInteger_mul",
                sNumberPrimitives.multiply);
        smallInteger.setPrimitive("SmallInteger_div", sNumberPrimitives.divide);
        smallInteger
                .setPrimitive("SmallInteger_lt", sNumberPrimitives.lessThen);
        smallInteger.setPrimitive("SmallInteger_gt",
                sNumberPrimitives.greaterThen);
        smallInteger.setPrimitive("SmallInteger_le",
                sNumberPrimitives.lessEqual);
        smallInteger.setPrimitive("SmallInteger_ge",
                sNumberPrimitives.greaterEqual);
        smallInteger.setPrimitive("SmallInteger_eq", sNumberPrimitives.equal);
        smallInteger
                .setPrimitive("SmallInteger_ne", sNumberPrimitives.notEqual);
        smallInteger.setPrimitive("SmallInteger_mod", sNumberPrimitives.mod);

        smallInteger.setPrimitive("SmallInteger_bitAnd", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STSmallInteger first = receiver.castToSubclass();

                STSmallInteger second = getStrictCastedObject(routine,
                        routine.getArgument(0), "SmallInteger");

                return first.bitAnd(second);
            }
        });

        smallInteger.setPrimitive("SmallInteger_bitOr", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STSmallInteger first = receiver.castToSubclass();

                STSmallInteger second = getStrictCastedObject(routine,
                        routine.getArgument(0), "SmallInteger");

                return first.bitOr(second);
            }
        });

        smallInteger.setPrimitive("SmallInteger_bitXor", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STSmallInteger first = receiver.castToSubclass();

                STSmallInteger second = getStrictCastedObject(routine,
                        routine.getArgument(0), "SmallInteger");

                return first.bitXor(second);
            }
        });

        smallInteger.setPrimitive("SmallInteger_bitShift", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STSmallInteger first = receiver.castToSubclass();

                STSmallInteger second = getStrictCastedObject(routine,
                        routine.getArgument(0), "SmallInteger");

                return first.bitShift(second);
            }
        });

        smallInteger.setPrimitive("SmallInteger_asFloat", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STSmallInteger first = receiver.castToSubclass();

                return first.asFloat();
            }
        });

        smallInteger.setPrimitive("SmallInteger_asLargeInteger",
                new STPrimitive() {
                    @Override
                    protected STObject onExecute(Routine routine,
                            STObject receiver, STStack stack) {
                        STSmallInteger first = receiver.castToSubclass();

                        return first.asLargeInteger();
                    }
                });

        smallInteger.setPrimitive("SmallInteger_newColon", new STPrimitive() {
            @Override
            protected STObject onExecute(Routine routine, STObject receiver,
                    STStack stack) {
                STNumber number = getStrictCastedObject(routine,
                        routine.getArgument(0), "Number");
                STNumber result = number
                        .convert(STNumber.SMALL_INTEGER_PRIORITY);
                if (result == null) {
                    primitiveError(routine,
                            "Can`t create SmallInteger instance from %s ",
                            number.toString());
                }

                return result;
            }
        });

    }
}

// SyxPrimitiveEntry _syx_primitive_entries[] = {
// { "Processor_yield", Processor_yield },
//
// /* Common for objects */

// !!!!!!!!!!!!!!!!!!!!!
// { "Object_at", Object_at },
// { "Object_at_put", Object_at_put },
// { "ArrayedCollection_replaceFromToWithStartingAt",
// ArrayedCollection_replaceFromToWithStartingAt },
// { "LargeInteger_intDiv", LargeInteger_intDiv },
// { "LargeInteger_quo", LargeInteger_quo },
// !!!!!!!!!!!!!!!!!!!!

//

//
// /* Contexts */
// { "ContextPart_parent", ContextPart_parent },
// { "ContextPart_receiver", ContextPart_receiver },
// { "BlockContext_outerContext", BlockContext_outerContext },
//
// /* Interpreter */
// { "Processor_enter", Processor_enter },
// { "Processor_swapWith", Processor_swapWith },
// { "Processor_leaveTo_andAnswer", Processor_leaveTo_andAnswer },
//
// { "Semaphore_signal", Semaphore_signal },
// { "Semaphore_wait", Semaphore_wait },
// { "Semaphore_waitFor", Semaphore_waitFor },
//

//
// /* File streams */
// { "StdIOStream_nextPut", StdIOStream_nextPut },
// { "StdIOStream_nextPutAll", StdIOStream_nextPutAll },
// { "FileStream_fileOp", FileStream_fileOp },
