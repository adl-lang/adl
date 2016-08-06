package adl.test;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import java.util.ArrayList;
import java.util.Objects;

public class Tree<T> {

  /* Members */

  private T value;
  private ArrayList<Tree<T>> children;

  /* Constructors */

  public Tree(T value, ArrayList<Tree<T>> children) {
    this.value = Objects.requireNonNull(value);
    this.children = Objects.requireNonNull(children);
  }

  /* Accessors and mutators */

  public T getValue() {
    return value;
  }

  public void setValue(T newValue) {
    value = Objects.requireNonNull(newValue);
  }

  public ArrayList<Tree<T>> getChildren() {
    return children;
  }

  public void setChildren(ArrayList<Tree<T>> newChildren) {
    children = Objects.requireNonNull(newChildren);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Tree)) {
      return false;
    }
    Tree other = (Tree) other0;
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
      final Factory<ArrayList<Tree<T>>> children = Factories.arrayList(Tree.factory(factoryT));

      public Tree<T> create() {
        return new Tree<T>(value.create(), children.create());
      }

      public Tree<T> create(Tree<T> other) {
        return new Tree<T>(value.create(other.getValue()), children.create(other.getChildren()));
      }
    };
  }
}
