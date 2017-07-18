/* Code generated from adl module test3 */

package adl.test3;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import java.util.Objects;

@SuppressWarnings("unused")
public class A {

  /* Members */

  private short f_int;
  private String f_string;
  private boolean f_bool;

  /* Constructors */

  public A(short f_int, String f_string, boolean f_bool) {
    this.f_int = f_int;
    this.f_string = Objects.requireNonNull(f_string);
    this.f_bool = f_bool;
  }

  public A() {
    this.f_int = (short)0;
    this.f_string = "";
    this.f_bool = false;
  }

  public A(A other) {
    this.f_int = other.f_int;
    this.f_string = other.f_string;
    this.f_bool = other.f_bool;
  }

  /* Accessors and mutators */

  public short getF_int() {
    return f_int;
  }

  public void setF_int(short f_int) {
    this.f_int = f_int;
  }

  public String getF_string() {
    return f_string;
  }

  public void setF_string(String f_string) {
    this.f_string = Objects.requireNonNull(f_string);
  }

  public boolean getF_bool() {
    return f_bool;
  }

  public void setF_bool(boolean f_bool) {
    this.f_bool = f_bool;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof A)) {
      return false;
    }
    A other = (A) other0;
    return
      f_int == other.f_int &&
      f_string.equals(other.f_string) &&
      f_bool == other.f_bool;
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + (int) f_int;
    _result = _result * 37 + f_string.hashCode();
    _result = _result * 37 + (f_bool ? 0 : 1);
    return _result;
  }

  /* Factory for construction of generic values */

  public static final Factory<A> FACTORY = new Factory<A>() {
    @Override
    public A create() {
      return new A();
    }

    @Override
    public A create(A other) {
      return new A(other);
    }
  };

  /* Json serialization */

  public static JsonBinding<A> jsonBinding() {
    final Lazy<JsonBinding<Short>> f_int = new Lazy<>(() -> JsonBindings.SHORT);
    final Lazy<JsonBinding<String>> f_string = new Lazy<>(() -> JsonBindings.STRING);
    final Lazy<JsonBinding<Boolean>> f_bool = new Lazy<>(() -> JsonBindings.BOOLEAN);
    final Factory<A> _factory = FACTORY;

    return new JsonBinding<A>() {
      public Factory<A> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(A _value) {
        JsonObject _result = new JsonObject();
        _result.add("f_int", f_int.get().toJson(_value.f_int));
        _result.add("f_string", f_string.get().toJson(_value.f_string));
        _result.add("f_bool", f_bool.get().toJson(_value.f_bool));
        return _result;
      }

      @Override
      public A fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new A(
          JsonBindings.fieldFromJson(_obj, "f_int", f_int.get()),
          JsonBindings.fieldFromJson(_obj, "f_string", f_string.get()),
          _obj.has("f_bool") ? JsonBindings.fieldFromJson(_obj, "f_bool", f_bool.get()) : false
        );
      }
    };
  }
}
