/* @generated from adl module test5 */

package adl.test5;

import com.google.gson.JsonElement;
import com.google.gson.JsonPrimitive;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonParseException;
import org.adl.runtime.sys.adlast.ScopedName;
import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.sys.adlast.TypeRef;
import java.util.ArrayList;

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
    @Override
    public U1 create() {
      return V;
    }

    @Override
    public U1 create(U1 other) {
      return other;
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test5", "U1");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
  };

  /* Json serialization */

  public static JsonBinding<U1> jsonBinding() {
    return new JsonBinding<U1>() {
      @Override
      public Factory<U1> factory() {
        return FACTORY;
      }

      @Override
      public JsonElement toJson(U1 _value) {
        return new JsonPrimitive(_value.toString());
      }

      @Override
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
