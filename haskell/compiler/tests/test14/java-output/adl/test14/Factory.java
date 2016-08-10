package adl.test14;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import java.util.Objects;

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

  public void setValue(String newValue) {
    value = Objects.requireNonNull(newValue);
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
    int result = 1;
    result = result * 37 + value.hashCode();
    return result;
  }

  /* Factory for construction of generic values */

  public static final org.adl.runtime.Factory<Factory> FACTORY = new org.adl.runtime.Factory<Factory>() {
    public Factory create() {
      return new Factory();
    }
    public Factory create(Factory other) {
      return new Factory(other);
    }
  };

  /* Json serialization */

  public static JsonBinding<Factory> jsonBinding() {
    final JsonBinding<String> value = JsonBindings.STRING;
    final org.adl.runtime.Factory<Factory> _factory = FACTORY;

    return new JsonBinding<Factory>() {
      public org.adl.runtime.Factory<Factory> factory() {
        return _factory;
      }

      public JsonElement toJson(Factory _value) {
        JsonObject _result = new JsonObject();
        _result.add("value", value.toJson(_value.value));
        return _result;
      }

      public Factory fromJson(JsonElement _json) {
        JsonObject _obj = _json.getAsJsonObject();
        return new Factory(
          _obj.has("value") ? value.fromJson(_obj.get("value")) : ""
        );
      }
    };
  }
}
