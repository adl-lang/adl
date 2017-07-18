/* Code generated from adl module test14 */

package adl.test14;

import com.google.gson.JsonElement;
import com.google.gson.JsonPrimitive;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonParseException;

public enum unsigned {

  /* Members */

  NULL_;

  @Override
  public String toString() {
    switch(this) {
      case NULL_: return "null";
    }
    throw new IllegalArgumentException();
  }

  public static unsigned fromString(String s) {
    if (s.equals("null")) {
      return NULL_;
    }
    throw new IllegalArgumentException("illegal value: " + s);
  }

  public static final Factory<unsigned> FACTORY = new Factory<unsigned>() {
    @Override
    public unsigned create() {
      return NULL_;
    }

    @Override
    public unsigned create(unsigned other) {
      return other;
    }
  };

  /* Json serialization */

  public static JsonBinding<unsigned> jsonBinding() {
    return new JsonBinding<unsigned>() {
      public Factory<unsigned> factory() {
        return FACTORY;
      }

      @Override
      public JsonElement toJson(unsigned _value) {
        return new JsonPrimitive(_value.toString());
      }

      @Override
      public unsigned fromJson(JsonElement _json) {
        try {
          return fromString(_json.getAsString());
        } catch (IllegalArgumentException e) {
          throw new JsonParseException(e.getMessage());
        }
      }
    };
  }
}
