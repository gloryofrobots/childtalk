package ua.ho.gloryofrobots.yellowtalk.node;

public interface NodeWithMetaData {
    public class UnknownMetaDataException extends Exception {
   	private static final long serialVersionUID = 6121381115947607722L;
   	public UnknownMetaDataException(String txt) {
   	    super(txt);
   	}
       }
    
    public void setMetaData(String label, String value) throws UnknownMetaDataException;
}
