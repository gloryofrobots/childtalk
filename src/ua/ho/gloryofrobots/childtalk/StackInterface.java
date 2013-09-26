package ua.ho.gloryofrobots.childtalk;


public interface StackInterface<T> {
    
    public  void push(T obj); 
    

    public  T pop();
    

    public  int getCurrentPosition();

    public  void setIndex(int position);
    

    public  void put(int position, T value);
}