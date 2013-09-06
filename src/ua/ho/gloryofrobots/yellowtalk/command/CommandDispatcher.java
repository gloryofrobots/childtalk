package ua.ho.gloryofrobots.yellowtalk.command;

public class CommandDispatcher {
    static public class Entry {
        void visit(Visitor visitor) {
            visitor.visit(this);
        }
    }

    static public class EntryA extends Entry {
        public void test() {
            System.out.println("A");
        }
    }

    static public class EntryB extends Entry {
        public void test() {
            System.out.println("B");
        }
    }

    static public class EntryC extends Entry {
        public void test() {
            System.out.println("C");
        }
    }

    static public interface Visitor {
       
        void visit(Entry entry);
        void visit(EntryA entry);
        void visit(EntryB entry);

        void visit(EntryC entry);
    }

    static public interface VisitorA extends Visitor {
        void visit(EntryA entry);
    }

    static public interface VisitorBC extends Visitor {
        void visit(EntryB entry);

        void visit(EntryC entry);
    }

    public static void main(String[] args) {
      
        Visitor visitor = new Visitor() {

            @Override
            public void visit(Entry entry) {
                System.out.println("VISITOR  DEFAULT");
            }

            @Override
            public void visit(EntryA entry) {
                entry.test();
            }

            @Override
            public void visit(EntryB entry) {
                entry.test();
                
            }

            @Override
            public void visit(EntryC entry) {
                entry.test();
                
            }

        };

//        VisitorA visitorA = new VisitorA() {
//
//            @Override
//            public void visit(Entry entry) {
//                System.out.println("VISITOR A DEFAULT");
//            }
//
//            @Override
//            public void visit(EntryA entry) {
//                entry.test();
//            }
//
//        };
//
//        VisitorBC visitorBC = new VisitorBC() {
//
//            @Override
//            public void visit(Entry entry) {
//                System.out.println("VISITOR BC DEFAULT");
//            }
//
//            @Override
//            public void visit(EntryB entry) {
//                entry.test();
//
//            }
//
//            @Override
//            public void visit(EntryC entry) {
//                entry.test();
//            }
//
//        };
        
        

        Entry e = new Entry();
        EntryA a = new EntryA();
        EntryB b = new EntryB();
        EntryC c = new EntryC();
        System.out.println("VISITOR A TEST");
        e.visit(visitor);
        a.visit(visitor);
        b.visit(visitor);
        c.visit(visitor);
       
        
    }

    public class UnsupportedCommandException extends Exception {
        private static final long serialVersionUID = 1L;

    }

    public void onExecute(CommandEntry commandEntry)
            throws UnsupportedCommandException {
        throw new UnsupportedCommandException();
    }

}
