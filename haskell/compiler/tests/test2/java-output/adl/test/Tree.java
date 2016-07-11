package adl.test;

public class Tree<T> {

  public T value;
  public java.util.ArrayList<Tree<T>> children;

  public Tree(T value, java.util.ArrayList<Tree<T>> children) {
    this.value = value;
    this.children = children;
  }
}