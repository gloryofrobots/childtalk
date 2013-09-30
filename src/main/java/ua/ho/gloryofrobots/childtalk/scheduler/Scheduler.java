package ua.ho.gloryofrobots.childtalk.scheduler;
import ua.ho.gloryofrobots.childtalk.stobject.STProcess;

public class Scheduler {
    
    private class ProcessEntry {
        ProcessEntry(STProcess process) {
            this.process = process;
        }
       
        STProcess process;
        ProcessEntry next;
        ProcessEntry previous;
    }
    
    ProcessEntry mHead;
    ProcessEntry mCurrent;
    private boolean mIsEnable;
    public Scheduler() {
        mIsEnable = false;
    }
    
    public void enable() {
        if(mIsEnable == true){
           return;
        } 
        
        mIsEnable = true;
        run();
    }
    
    public void disable() {
        mIsEnable = false;
    }
    
    public boolean isEnabled() {
        return mIsEnable;
    }
    
    public void addProcess(STProcess process) {
        if(mHead == null) {
            mHead = new ProcessEntry(process);
            return;
        }
        
        ProcessEntry entry = new ProcessEntry(process);
        entry.next = mHead;
        mHead.previous = entry;
        mHead = entry;
    }
    
    public void run() {
        
        while(true) {
            if(isEnabled() == false) {
                break;
            }
            
            if(mHead == null) {
                break;
            }
            
            executeProcesses();
        }
        
        disable();
    }
    
    private void executeProcesses() {
        ProcessEntry current = mHead;
        while(true) {
            if(isEnabled() == false) {
                break;
            }
            if(current == null) {
                break;
            }
            
            STProcess process = current.process;
            STProcess.State state = process.getState();
            if(state == STProcess.State.SUSPENDED) {
                current = current.next;
                continue;
            } else if(state == STProcess.State.TERMINATED) {
                current = killProcess(current);
                continue;
            } else if(state == STProcess.State.IDLE) {
                process.activate();   
            }
           
            if(process.isActive()) {
                try {
                    process.execute();
                } catch (Exception e) {
                    killProcess(current);
                    disable();
                    throw e;
                }
            }
            
            current = current.next;
        }
    }
    
    public STProcess getCurrentProcess() {
        return mHead.process;
    }
    //return next entry
    private ProcessEntry killProcess(ProcessEntry entry) {
        ProcessEntry previous = entry.previous;
        ProcessEntry next = entry.next;
        if(previous != null && next != null) {
            previous.next = next;
            next.previous = previous;
            return next;
        } else if(previous == null && next != null) {
            next.previous = null;
            mHead = next;
            return next;
        } else if(next == null && previous != null) {
            previous.next = null;
            return null;
        } else {
            mHead = null;
            return null;
        }
    }
}
