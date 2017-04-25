/* Code generated from adl module test2 */

package adl.test2;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import java.util.Objects;

/**
 * An empty structure.
 */
public class S0 {


  /* Constructors */

  public S0() {
  }

  public S0(S0 other) {
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other) {
    return other instanceof S0;
  }

  @Override
  public int hashCode() {
    int _result = 1;
    return _result;
  }

  /* Factory for construction of generic values */

  public static final Factory<S0> FACTORY = new Factory<S0>() {
    public S0 create() {
      return new S0();
    }
    public S0 create(S0 other) {
      return new S0(other);
    }
  };

  /* Json serialization */

  public static JsonBinding<S0> jsonBinding() {
    final Factory<S0> _factory = FACTORY;

    return new JsonBinding<S0>() {
      public Factory<S0> factory() {
        return _factory;
      }

      public JsonElement toJson(S0 _value) {
        JsonObject _result = new JsonObject();
        return _result;
      }

      public S0 fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new S0(
        );
      }
    };
  }
}
