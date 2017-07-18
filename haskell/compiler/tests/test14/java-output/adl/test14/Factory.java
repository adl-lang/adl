/* Code generated from adl module test14 */

package adl.test14;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import java.util.Objects;

@SuppressWarnings("unused")
public class Factory {

  /* Members */

  private String value;

  /* Constructors */

  public Factory(String value) {
    this.value = Objects.requireNonNull(value);
  }

  public Factory() {
    this.value = "";
  }

  public Factory(Factory other) {
    this.value = other.value;
  }

  /* Accessors and mutators */

  public String getValue() {
    return value;
  }

  public void setValue(String value) {
    this.value = Objects.requireNonNull(value);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Factory)) {
      return false;
    }
    Factory other = (Factory) other0;
    return
      value.equals(other.value);
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + value.hashCode();
    return _result;
  }

  /* Factory for construction of generic values */

  public static final org.adl.runtime.Factory<Factory> FACTORY = new org.adl.runtime.Factory<Factory>() {
    @Override
    public Factory create() {
      return new Factory();
    }

    @Override
    public Factory create(Factory other) {
      return new Factory(other);
    }
  };

  /* Json serialization */

  public static JsonBinding<Factory> jsonBinding() {
    final JsonBinding<String> _binding = JsonBindings.STRING;
    final org.adl.runtime.Factory<Factory> _factory = FACTORY;

    return new JsonBinding<Factory>() {
      public org.adl.runtime.Factory<Factory> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(Factory _value) {
        return _binding.toJson(_value.value);
      }

      @Override
      public Factory fromJson(JsonElement _json) {
        return new Factory(_binding.fromJson(_json));
      }
    };
  }
}
