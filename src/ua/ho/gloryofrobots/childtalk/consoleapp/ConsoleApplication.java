package ua.ho.gloryofrobots.childtalk.consoleapp;

import java.io.InputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.NoSuchElementException;
import java.util.Scanner;

import ua.ho.gloryofrobots.childtalk.ChildtalkApplicationInterface;
import ua.ho.gloryofrobots.childtalk.bootstrap.BootstrapSuite;
import ua.ho.gloryofrobots.childtalk.bootstrap.ImageSuite;
import ua.ho.gloryofrobots.childtalk.compilation.CompileSuite;
import ua.ho.gloryofrobots.childtalk.scheduler.SchedulingSuite;
import ua.ho.gloryofrobots.childtalk.stobject.STObject;

public class ConsoleApplication {

    static class Options {
        boolean runTests;
        boolean buildDefaultImage;

        boolean loadImage;
        String imagePath;
        boolean runFiles;
        ArrayList<String> filesToRun;
        boolean runSmallTalk;

        boolean showHelp;
    }

    ChildtalkApplicationInterface mApplication = new ChildtalkApplicationInterface() {
        @Override
        public void onQuit() {
            System.exit(0);
        }

        @Override
        public PrintStream getOutputStream() {
            return System.out;
        }

        @Override
        public InputStream getInputStream() {
            return System.in;
        }

        @Override
        public PrintStream getErrorStream() {
            return System.err;
        }
    };

    private boolean mInitialised;

    private boolean isInitialised() {
        return mInitialised;
    }

    private void showHelp() {
        StringBuilder builder = new StringBuilder();
        String separator = System.getProperty("line.separator");
        builder.append("ChildTalk version 1.0");
        builder.append(separator);
        builder.append("Usage:");
        builder.append(separator);

        builder.append("-t");
        builder.append("          ");
        builder.append("Tun Tests");
        builder.append(separator);

        builder.append("-i FILEPATH");
        builder.append("          ");
        builder.append("Load image from FILEPATH");
        builder.append(separator);

        builder.append("-d");
        builder.append("          ");
        builder.append("Build default image from scratch");
        builder.append(separator);

        builder.append("-e CODE");
        builder.append("          ");
        builder.append("Eval line of code");
        builder.append(separator);

        builder.append("-r");
        builder.append("          ");
        builder.append("Run Childtalk after loading files or evaluating code");
        builder.append(separator);

        builder.append("-v");
        builder.append("          ");
        builder.append("Print version");
        builder.append(separator);

        builder.append("-h");
        builder.append("          ");
        builder.append("Print this page");
        builder.append(separator);

        builder.append("-f FILEPATH1, FILEPATH2");
        builder.append("          ");

        builder.append("Load and eval files. For Example -f \"/usr/share/Planet.st\" \"/usr/share/Earth.st\"");
        builder.append(separator);
    }

    public Options parseArguments(String[] args) {
        Options opts = new Options();
        if (args.length == 0) {
            opts.runSmallTalk = true;
            return opts;
        }

        int i = 0;
        while (i < args.length) {
            String arg = args[i];
            char check = arg.charAt(0);
            if (check != '-') {
                error("Error parsing arguments.");
            }
            char flag = arg.charAt(1);
            if (flag == 'v') {
                opts.showHelp = true;
            } else if (flag == 'f') {
                opts.runFiles = true;
                i = getArgValues(opts.filesToRun, i, args);
                if (i == -1) {
                    error("Error parsing files argument");
                }
            } else if (flag == 'i') {
                opts.loadImage = true;
                i++;
                opts.imagePath = args[i];
            } else if (flag == 't') {
                opts.runTests = true;
            } else if (flag == 'r') {
                opts.runSmallTalk = true;
            } else if (flag == 'd') {
                opts.buildDefaultImage = true;
            } else {
                error("Unknown argument -" + flag);
            }
            i++;
        }

        return opts;
    }

    private int getArgValues(ArrayList<String> dest, int i, String[] args) {
        while (i < args.length) {
            String arg = args[i];
            char check = arg.charAt(0);
            if (check == '-') {
                i--;
                return i;
            }
            dest.add(arg);
        }

        return -1;
    }

    public void run(String[] args) {
        Options opts = parseArguments(args);
        _run(opts);
    }

    public void _run(Options opts) {
        BootstrapSuite.setApplication(mApplication);

        if (opts.showHelp) {
            showHelp();
            return;
        }

        if (opts.buildDefaultImage) {
            buildDefaultImage();
        }

        if (opts.loadImage) {
            loadImage(opts.imagePath);
        }

        if (!isInitialised()) {
            loadDefaultImage();
        }
        BootstrapSuite.bootstrap();
        
        if (opts.runFiles) {
            runFiles(opts.filesToRun);
        }

        if (opts.runTests) {
            runTests();
        }

        if (opts.runSmallTalk) {
            runSmallTalk();
        }
    }

    private void runTests() {
        SmallTalkTest test = new SmallTalkTest();
        test.run();
    }

    private void print(String line) {
        System.out.print(line);
    }

    private void println(String line) {
        System.out.println(line);
    }

    private void runSmallTalk() {
        Scanner scanner = new Scanner(System.in);
        while (true) {
            int status = evalLine(scanner);

            if (status <= 0) {
                System.exit(status);
            }
        }
    }

    private int evalLine(Scanner scanner) {
        print("> ");
        try {
            String line = scanner.nextLine();
            STObject result = CompileSuite.eval(line);
            if(result == ImageSuite.image().objects().NIL) {
                println("");
            } else {
                SchedulingSuite.callSelectorInNewProcess(ImageSuite.image().symbols().PRINTNL, result.getSTClass(), result);
            }
            //println(result.toString());
            
            return 1;
        } catch (NoSuchElementException e) {
            return 0;
        } catch(RuntimeException e) {
            //println(e.getMessage());
            //e.printStackTrace();
            return 1;
        }
    }

    private void runFiles(ArrayList<String> filesToRun) {
        try {
            String[] arr = (String[]) filesToRun.toArray();
            BootstrapSuite.loadAndEvalFiles(arr);
        } catch (RuntimeException e) {
            println(e.getMessage());
            //e.printStackTrace();
            error("Error evaluate file %s", e.getMessage());
        }
    }

    String defaultImagePath = "/home/gloryofrobots/develop/smalltalk/childtalk/image/default.sim";

    private void loadDefaultImage() {
        loadImage(defaultImagePath);
    }

    public void error(String format, Object... args) {
        String txt = String.format(format, args);
        System.err.println(txt);
        System.exit(-1);
    }

    private void loadImage(String imagePath) {
        boolean result = BootstrapSuite.loadImage(imagePath);
        if (!result) {
            error("Image loading error %s", imagePath);
        }
    }

    private void buildDefaultImage() {
        try {
            BootstrapSuite
                    .buildDefaultImage("/home/gloryofrobots/develop/smalltalk/childtalk/st/core");
        } catch (RuntimeException e) {
            e.printStackTrace();
            error("Error building default image %s", e.getMessage());
        }
        if (BootstrapSuite.saveCurrentImage(defaultImagePath) == false) {
            error("Error saving image %s", defaultImagePath);
        }
        mInitialised = true;
    }

    public static void main(String[] args) {
        ConsoleApplication app = new ConsoleApplication();
        String[] arguments = { "-r", "-t" };
        app.run(arguments);
    }
}
