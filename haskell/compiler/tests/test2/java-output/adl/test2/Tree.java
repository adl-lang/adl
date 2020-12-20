/* @generated from adl module test2 */

package adl.test2;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Builders;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import org.adl.runtime.sys.adlast.ScopedName;
import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.sys.adlast.TypeRef;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class Tree<T> {

  /* Members */

  private T value;
  private List<Tree<T>> children;

  /* Constructors */

  public Tree(T value, List<Tree<T>> children) {
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

  public List<Tree<T>> getChildren() {
    return children;
  }

  public void setChildren(List<Tree<T>> children) {
    this.children = Objects.requireNonNull(children);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Tree)) {
      return false;
    }
    Tree<?> other = (Tree<?>) other0;
    return
      value.equals(other.value) &&
      children.equals(other.children);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + value.hashCode();
    _result = _result * 37 + children.hashCode();
    return _result;
  }

  /* Builder */

  public static class Builder<T> {
    private T value;
    private List<Tree<T>> children;

    public Builder() {
      this.value = null;
      this.children = null;
    }

    public Builder<T> setValue(T value) {
      this.value = Objects.requireNonNull(value);
      return this;
    }

    public Builder<T> setChildren(List<Tree<T>> children) {
      this.children = Objects.requireNonNull(children);
      return this;
    }

    public Tree<T> create() {
      Builders.checkFieldInitialized("Tree", "value", value);
      Builders.checkFieldInitialized("Tree", "children", children);
      return new Tree<T>(value, children);
    }
  }

  /* Factory for construction of generic values */

  public static <T> Factory<Tree<T>> factory(Factory<T> factoryT) {
    return new Factory<Tree<T>>() {
      final Lazy<Factory<T>> value = new Lazy<>(() -> factoryT);
      final Lazy<Factory<List<Tree<T>>>> children = new Lazy<>(() -> Factories.list(Tree.factory(factoryT)));

      @Override
      public Tree<T> create(Tree<T> other) {
        return new Tree<T>(
          value.get().create(other.getValue()),
          children.get().create(other.getChildren())
          );
      }

      @Override
      public TypeExpr typeExpr() {
        ScopedName scopedName = new ScopedName("test2", "Tree");
        ArrayList<TypeExpr> params = new ArrayList<>();
        params.add(factoryT.typeExpr());
        return new TypeExpr(TypeRef.reference(scopedName), params);
      }

      @Override
      public JsonBinding<Tree<T>> jsonBinding() {
        return Tree.jsonBinding(factoryT.jsonBinding());
      }
    };
  }

  /* Json serialization */

  public static<T> JsonBinding<Tree<T>> jsonBinding(JsonBinding<T> bindingT) {
    final Lazy<JsonBinding<T>> value = new Lazy<>(() -> bindingT);
    final Lazy<JsonBinding<List<Tree<T>>>> children = new Lazy<>(() -> JsonBindings.list(Tree.jsonBinding(bindingT)));
    final Factory<T> factoryT = bindingT.factory();
    final Factory<Tree<T>> _factory = factory(bindingT.factory());

    return new JsonBinding<Tree<T>>() {
      @Override
      public Factory<Tree<T>> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(Tree<T> _value) {
        JsonObject _result = new JsonObject();
        _result.add("value", value.get().toJson(_value.value));
        _result.add("children", children.get().toJson(_value.children));
        return _result;
      }

      @Override
      public Tree<T> fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new Tree<T>(
          JsonBindings.fieldFromJson(_obj, "value", value.get()),
          JsonBindings.fieldFromJson(_obj, "children", children.get())
        );
      }
    };
  }
}
