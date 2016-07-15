package adl.test;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;

public class Tree<T> {

  private T value;
  private java.util.ArrayList<Tree<T>> children;

  public Tree(T value, java.util.ArrayList<Tree<T>> children) {
    this.value = java.util.Objects.requireNonNull(value);
    this.children = java.util.Objects.requireNonNull(children);
  }

  public T getValue() {
    return value;
  }

  public void setValue(T newValue) {
    value = newValue;
  }

  public java.util.ArrayList<Tree<T>> getChildren() {
    return children;
  }

  public void setChildren(java.util.ArrayList<Tree<T>> newChildren) {
    children = newChildren;
  }

  public boolean equals(Tree other) {
    return
      value.equals(other.value) &&
      children.equals(other.children);
  }

  public int hashCode() {
    int result = 1;
    result = result * 37 + value.hashCode();
    result = result * 37 + children.hashCode();
    return result;
  }

  public static <T> Factory<Tree<T>> factory(Factory<T> factoryT) {
    return new Factory<Tree<T>>() {
      final Factory<T> value = factoryT;
      final Factory<java.util.ArrayList<Tree<T>>> children = Factories.ArrayListFactory(Tree.factory(factoryT));

      public Tree<T> create() {
        return new Tree<T>(value.create(),children.create());
      }

      public Tree<T> create(Tree<T> other) {
        return new Tree<T>(value.create(other.getValue()),children.create(other.getChildren()));
      }
    };
  }
}