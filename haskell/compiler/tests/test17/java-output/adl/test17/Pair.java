/* Code generated from adl module test17 */

package adl.test17;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import java.util.Objects;

public class Pair<A, B> {

  /* Members */

  private A v1;
  private B v2;

  /* Constructors */

  public Pair(A v1, B v2) {
    this.v1 = Objects.requireNonNull(v1);
    this.v2 = Objects.requireNonNull(v2);
  }

  /* Accessors and mutators */

  public A getV1() {
    return v1;
  }

  public void setV1(A v1) {
    this.v1 = Objects.requireNonNull(v1);
  }

  public B getV2() {
    return v2;
  }

  public void setV2(B v2) {
    this.v2 = Objects.requireNonNull(v2);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Pair)) {
      return false;
    }
    Pair other = (Pair) other0;
    return
      v1.equals(other.v1) &&
      v2.equals(other.v2);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + v1.hashCode();
    _result = _result * 37 + v2.hashCode();
    return _result;
  }

  /* Factory for construction of generic values */

  public static <A, B> Factory<Pair<A, B>> factory(Factory<A> factoryA, Factory<B> factoryB) {
    return new Factory<Pair<A, B>>() {
      final Lazy<Factory<A>> v1 = new Lazy<>(() -> factoryA);
      final Lazy<Factory<B>> v2 = new Lazy<>(() -> factoryB);

      @Override
      public Pair<A, B> create() {
        return new Pair<A, B>(
          v1.get().create(),
          v2.get().create()
          );
      }

      @Override
      public Pair<A, B> create(Pair<A, B> other) {
        return new Pair<A, B>(
          v1.get().create(other.getV1()),
          v2.get().create(other.getV2())
          );
      }
    };
  }

  /* Json serialization */

  public static<A, B> JsonBinding<Pair<A, B>> jsonBinding(JsonBinding<A> bindingA, JsonBinding<B> bindingB) {
    final Lazy<JsonBinding<A>> v1 = new Lazy<>(() -> bindingA);
    final Lazy<JsonBinding<B>> v2 = new Lazy<>(() -> bindingB);
    final Factory<A> factoryA = bindingA.factory();
    final Factory<B> factoryB = bindingB.factory();
    final Factory<Pair<A, B>> _factory = factory(bindingA.factory(), bindingB.factory());

    return new JsonBinding<Pair<A, B>>() {
      public Factory<Pair<A, B>> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(Pair<A, B> _value) {
        JsonObject _result = new JsonObject();
        _result.add("v1", v1.get().toJson(_value.v1));
        _result.add("v2", v2.get().toJson(_value.v2));
        return _result;
      }

      @Override
      public Pair<A, B> fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new Pair<A, B>(
          JsonBindings.fieldFromJson(_obj, "v1", v1.get()),
          JsonBindings.fieldFromJson(_obj, "v2", v2.get())
        );
      }
    };
  }
}
