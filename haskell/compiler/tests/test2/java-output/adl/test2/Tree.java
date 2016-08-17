package adl.test2;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
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

  public void setValue(T value) {
    this.value = Objects.requireNonNull(value);
  }

  public ArrayList<Tree<T>> getChildren() {
    return children;
  }

  public void setChildren(ArrayList<Tree<T>> children) {
    this.children = Objects.requireNonNull(children);
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
        return new Tree<T>(
          value.create(),
          children.create()
          );
      }

      public Tree<T> create(Tree<T> other) {
        return new Tree<T>(
          value.create(other.getValue()),
          children.create(other.getChildren())
          );
      }
    };
  }

  /* Json serialization */

  public static<T> JsonBinding<Tree<T>> jsonBinding(JsonBinding<T> bindingT) {
    final JsonBinding<T> value = bindingT;
    final JsonBinding<ArrayList<Tree<T>>> children = JsonBindings.arrayList(adl.test2.Tree.jsonBinding(bindingT));
    final Factory<T> factoryT = bindingT.factory();
    final Factory<Tree<T>> _factory = factory(bindingT.factory());

    return new JsonBinding<Tree<T>>() {
      public Factory<Tree<T>> factory() {
        return _factory;
      }

      public JsonElement toJson(Tree<T> _value) {
        JsonObject _result = new JsonObject();
        _result.add("value", value.toJson(_value.value));
        _result.add("children", children.toJson(_value.children));
        return _result;
      }

      public Tree<T> fromJson(JsonElement _json) {
        JsonObject _obj = _json.getAsJsonObject();
        return new Tree<T>(
          _obj.has("value") ? value.fromJson(_obj.get("value")) : factoryT.create(),
          _obj.has("children") ? children.fromJson(_obj.get("children")) : new ArrayList<Tree<T>>()
        );
      }
    };
  }
}
