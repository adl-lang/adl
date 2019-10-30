/* @generated from adl module test14 */

package adl.test14;

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

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test14", "unsigned");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
    @Override
    public JsonBinding<unsigned> jsonBinding() {
      return unsigned.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<unsigned> jsonBinding() {
    return new JsonBinding<unsigned>() {
      @Override
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
