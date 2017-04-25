/* Code generated from adl module test5 */

package adl.test5;

import com.google.gson.JsonElement;
import com.google.gson.JsonPrimitive;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonParseException;

public enum U1 {

  /* Members */

  V;

  @Override
  public String toString() {
    switch(this) {
      case V: return "v";
    }
    throw new IllegalArgumentException();
  }

  public static U1 fromString(String s) {
    if (s.equals("v")) {
      return V;
    }
    throw new IllegalArgumentException("illegal value: " + s);
  }

  public static final Factory<U1> FACTORY = new Factory<U1>() {
    public U1 create() {
      return V;
    }

    public U1 create(U1 other) {
      return other;
    }
  };

  /* Json serialization */

  public static JsonBinding<U1> jsonBinding() {
    return new JsonBinding<U1>() {
      public Factory<U1> factory() {
        return FACTORY;
      }

      public JsonElement toJson(U1 _value) {
        return new JsonPrimitive(_value.toString());
      }

      public U1 fromJson(JsonElement _json) {
        try {
          return fromString(_json.getAsString());
        } catch (IllegalArgumentException e) {
          throw new JsonParseException(e.getMessage());
        }
      }
    };
  }
}
