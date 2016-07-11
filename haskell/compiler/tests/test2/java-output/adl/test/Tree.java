package adl.test;

public class Tree<T> {

  public T value;
  public java.util.ArrayList<Tree<T>> children;

  public Tree(T value, java.util.ArrayList<Tree<T>> children) {
    this.value = value;
    this.children = children;
  }

  public Tree(Class<T> classT) {
    try {
      this.value = classT.getDeclaredConstructor().newInstance();
      this.children = new java.util.ArrayList<Tree<T>>();
    }
    catch (ReflectiveOperationException e) {
      throw new RuntimeException(e);
    }
  }

  public Tree(Tree<T> other, Class<T> classT) {
    try {
      this.value = classT.getDeclaredConstructor(other.value.getClass()).newInstance(other.value);
      this.children = new java.util.ArrayList<Tree<T>>(other.children);
    }
    catch (ReflectiveOperationException e) {
      throw new RuntimeException(e);
    }
  }
}