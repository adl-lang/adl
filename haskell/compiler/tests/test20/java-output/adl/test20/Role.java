/* @generated from adl module test20 */

package adl.test20;

import com.google.gson.JsonElement;
import com.google.gson.JsonPrimitive;
import org.adl.runtime.AdlVoid;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonParseException;
import org.adl.runtime.sys.adlast.ScopedName;
import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.sys.adlast.TypeRef;
import java.util.ArrayList;

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
    @Override
    public Role create(Role other) {
      return other;
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test20", "Role");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
    @Override
    public JsonBinding<Role> jsonBinding() {
      return Role.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<Role> jsonBinding() {
    return new JsonBinding<Role>() {
      @Override
      public Factory<Role> factory() {
        return FACTORY;
      }

      @Override
      public JsonElement toJson(Role _value) {
        return new JsonPrimitive(_value.toString());
      }

      @Override
      public Role fromJson(JsonElement _json) {
        try {
          return fromString(_json.getAsString());
        } catch (IllegalArgumentException e) {
          throw new JsonParseException(e.getMessage());
        }
      }
    };
  }
}
