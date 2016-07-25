package adl.test;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;

public class Tree<T> {

  /* Members */

  private T value;
  private java.util.ArrayList<Tree<T>> children;

  /* Constructors */

  public Tree(T value, java.util.ArrayList<Tree<T>> children) {
    this.value = java.util.Objects.requireNonNull(value);
    this.children = java.util.Objects.requireNonNull(children);
  }

  /* Accessors and mutators */

  public T getValue() {
    return value;
  }

  public void setValue(T newValue) {
    value = java.util.Objects.requireNonNull(newValue);
  }

  public java.util.ArrayList<Tree<T>> getChildren() {
    return children;
  }

  public void setChildren(java.util.ArrayList<Tree<T>> newChildren) {
    children = java.util.Objects.requireNonNull(newChildren);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Tree)) {
      return false;
    }
    Tree other = (Tree)other0;
    return
      value.equals(other.value) &&
      children.equals(other.children);
  }

  @Override
  public int hashCode() {
    int result = 1;
    result = result * 37 + value.hashCode();
    result = result * 37 + children.hashCode();
    return result;
  }

  /* Factory for construction of generic values */

  public static <T> Factory<Tree<T>> factory(Factory<T> factoryT) {
    return new Factory<Tree<T>>() {
      final Factory<T> value = factoryT;
      final Factory<java.util.ArrayList<Tree<T>>> children = Factories.ArrayListFactory(Tree.factory(factoryT));

      public Tree<T> create() {
        return new Tree<T>(value.create(), children.create());
      }

      public Tree<T> create(Tree<T> other) {
        return new Tree<T>(value.create(other.getValue()), children.create(other.getChildren()));
      }
    };
  }
}