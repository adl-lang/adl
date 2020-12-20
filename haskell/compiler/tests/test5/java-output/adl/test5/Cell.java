/* @generated from adl module test5 */

package adl.test5;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Builders;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import org.adl.runtime.sys.adlast.ScopedName;
import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.sys.adlast.TypeRef;
import java.util.ArrayList;
import java.util.Objects;

public class Cell<T> {

  /* Members */

  private T head;
  private List<T> tail;

  /* Constructors */

  public Cell(T head, List<T> tail) {
    this.head = Objects.requireNonNull(head);
    this.tail = Objects.requireNonNull(tail);
  }

  /* Accessors and mutators */

  public T getHead() {
    return head;
  }

  public Cell<T> setHead(T head) {
    this.head = Objects.requireNonNull(head);
    return this;
  }

  public List<T> getTail() {
    return tail;
  }

  public Cell<T> setTail(List<T> tail) {
    this.tail = Objects.requireNonNull(tail);
    return this;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Cell)) {
      return false;
    }
    Cell<?> other = (Cell<?>) other0;
    return
      head.equals(other.head) &&
      tail.equals(other.tail);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + head.hashCode();
    _result = _result * 37 + tail.hashCode();
    return _result;
  }

  /* Builder */

  public static class Builder<T> {
    private T head;
    private List<T> tail;

    public Builder() {
      this.head = null;
      this.tail = null;
    }

    public Builder<T> setHead(T head) {
      this.head = Objects.requireNonNull(head);
      return this;
    }

    public Builder<T> setTail(List<T> tail) {
      this.tail = Objects.requireNonNull(tail);
      return this;
    }

    public Cell<T> create() {
      Builders.checkFieldInitialized("Cell", "head", head);
      Builders.checkFieldInitialized("Cell", "tail", tail);
      return new Cell<T>(head, tail);
    }
  }

  /* Factory for construction of generic values */

  public static <T> Factory<Cell<T>> factory(Factory<T> factoryT) {
    return new Factory<Cell<T>>() {
      final Lazy<Factory<T>> head = new Lazy<>(() -> factoryT);
      final Lazy<Factory<List<T>>> tail = new Lazy<>(() -> List.factory(factoryT));

      @Override
      public Cell<T> create(Cell<T> other) {
        return new Cell<T>(
          head.get().create(other.getHead()),
          tail.get().create(other.getTail())
          );
      }

      @Override
      public TypeExpr typeExpr() {
        ScopedName scopedName = new ScopedName("test5", "Cell");
        ArrayList<TypeExpr> params = new ArrayList<>();
        params.add(factoryT.typeExpr());
        return new TypeExpr(TypeRef.reference(scopedName), params);
      }

      @Override
      public JsonBinding<Cell<T>> jsonBinding() {
        return Cell.jsonBinding(factoryT.jsonBinding());
      }
    };
  }

  /* Json serialization */

  public static<T> JsonBinding<Cell<T>> jsonBinding(JsonBinding<T> bindingT) {
    final Lazy<JsonBinding<T>> head = new Lazy<>(() -> bindingT);
    final Lazy<JsonBinding<List<T>>> tail = new Lazy<>(() -> List.jsonBinding(bindingT));
    final Factory<T> factoryT = bindingT.factory();
    final Factory<Cell<T>> _factory = factory(bindingT.factory());

    return new JsonBinding<Cell<T>>() {
      @Override
      public Factory<Cell<T>> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(Cell<T> _value) {
        JsonObject _result = new JsonObject();
        _result.add("head", head.get().toJson(_value.head));
        _result.add("tail", tail.get().toJson(_value.tail));
        return _result;
      }

      @Override
      public Cell<T> fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new Cell<T>(
          JsonBindings.fieldFromJson(_obj, "head", head.get()),
          JsonBindings.fieldFromJson(_obj, "tail", tail.get())
        );
      }
    };
  }
}
