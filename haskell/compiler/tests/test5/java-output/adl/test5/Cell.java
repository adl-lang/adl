package adl.test5;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
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

  public void setHead(T head) {
    this.head = Objects.requireNonNull(head);
  }

  public List<T> getTail() {
    return tail;
  }

  public void setTail(List<T> tail) {
    this.tail = Objects.requireNonNull(tail);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Cell)) {
      return false;
    }
    Cell other = (Cell) other0;
    return
      head.equals(other.head) &&
      tail.equals(other.tail);
  }

  @Override
  public int hashCode() {
    int result = 1;
    result = result * 37 + head.hashCode();
    result = result * 37 + tail.hashCode();
    return result;
  }

  /* Factory for construction of generic values */

  public static <T> Factory<Cell<T>> factory(Factory<T> factoryT) {
    return new Factory<Cell<T>>() {
      final Factory<T> head = factoryT;
      final Factory<List<T>> tail = List.factory(factoryT);

      public Cell<T> create() {
        return new Cell<T>(
          head.create(),
          tail.create()
          );
      }

      public Cell<T> create(Cell<T> other) {
        return new Cell<T>(
          head.create(other.getHead()),
          tail.create(other.getTail())
          );
      }
    };
  }

  /* Json serialization */

  public static<T> JsonBinding<Cell<T>> jsonBinding(JsonBinding<T> bindingT) {
    final JsonBinding<T> head = bindingT;
    final JsonBinding<List<T>> tail = List.jsonBinding(bindingT);
    final Factory<T> factoryT = bindingT.factory();
    final Factory<Cell<T>> _factory = factory(bindingT.factory());

    return new JsonBinding<Cell<T>>() {
      public Factory<Cell<T>> factory() {
        return _factory;
      }

      public JsonElement toJson(Cell<T> _value) {
        JsonObject _result = new JsonObject();
        _result.add("head", head.toJson(_value.head));
        _result.add("tail", tail.toJson(_value.tail));
        return _result;
      }

      public Cell<T> fromJson(JsonElement _json) {
        JsonObject _obj = _json.getAsJsonObject();
        return new Cell<T>(
          _obj.has("head") ? head.fromJson(_obj.get("head")) : factoryT.create(),
          _obj.has("tail") ? tail.fromJson(_obj.get("tail")) : List.factory(factoryT).create()
        );
      }
    };
  }
}
