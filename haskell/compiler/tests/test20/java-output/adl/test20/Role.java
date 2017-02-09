package adl.test20;

import com.google.gson.JsonElement;
import com.google.gson.JsonPrimitive;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;

public enum Role {

  /* Members */

  UNDERLING,
  BOSS,
  SUPERBOSS;

  @Override
  public String toString() {
    switch(this) {
      case UNDERLING: return "u";
      case BOSS: return "b";
      case SUPERBOSS: return "sb";
    }
    throw new IllegalArgumentException();
  }

  public static Role fromString(String s) {
    if (s.equals("u")) {
      return UNDERLING;
    }
    if (s.equals("b")) {
      return BOSS;
    }
    if (s.equals("sb")) {
      return SUPERBOSS;
    }
    throw new IllegalArgumentException("illegal value: " + s);
  }

  public static final Factory<Role> FACTORY = new Factory<Role>() {
    public Role create() {
      return UNDERLING;
    }

    public Role create(Role other) {
      return other;
    }
  };

  /* Json serialization */

  public static JsonBinding<Role> jsonBinding() {
    return new JsonBinding<Role>() {
      public Factory<Role> factory() {
        return FACTORY;
      }

      public JsonElement toJson(Role _value) {
        return new JsonPrimitive(_value.toString());
      }

      public Role fromJson(JsonElement _json) {
        return fromString(_json.getAsString());
      }
    };
  }
}
