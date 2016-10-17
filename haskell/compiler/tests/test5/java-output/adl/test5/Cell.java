package adl.test5;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.Lazy;
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
      final Lazy<Factory<T>> head = new Lazy<>(() -> factoryT);
      final Lazy<Factory<List<T>>> tail = new Lazy<>(() -> List.factory(factoryT));

      public Cell<T> create() {
        return new Cell<T>(
          head.get().create(),
          tail.get().create()
          );
      }

      public Cell<T> create(Cell<T> other) {
        return new Cell<T>(
          head.get().create(other.getHead()),
          tail.get().create(other.getTail())
          );
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
      public Factory<Cell<T>> factory() {
        return _factory;
      }

      public JsonElement toJson(Cell<T> _value) {
        JsonObject _result = new JsonObject();
        _result.add("head", head.get().toJson(_value.head));
        _result.add("tail", tail.get().toJson(_value.tail));
        return _result;
      }

      public Cell<T> fromJson(JsonElement _json) {
        JsonObject _obj = _json.getAsJsonObject();
        return new Cell<T>(
          _obj.has("head") ? head.get().fromJson(_obj.get("head")) : factoryT.create(),
          _obj.has("tail") ? tail.get().fromJson(_obj.get("tail")) : List.factory(factoryT).create()
        );
      }
    };
  }
}
