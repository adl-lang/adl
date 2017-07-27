/* Code generated from adl module test3 */

package adl.test3;

import com.google.gson.JsonElement;
import com.google.gson.JsonPrimitive;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonParseException;
import org.adl.sys.adlast.ScopedName;
import org.adl.sys.adlast.TypeExpr;
import org.adl.sys.adlast.TypeRef;
import java.util.ArrayList;

public enum E {

  /* Members */

  V1,
  V2;

  @Override
  public String toString() {
    switch(this) {
      case V1: return "v1";
      case V2: return "v2";
    }
    throw new IllegalArgumentException();
  }

  public static E fromString(String s) {
    if (s.equals("v1")) {
      return V1;
    }
    if (s.equals("v2")) {
      return V2;
    }
    throw new IllegalArgumentException("illegal value: " + s);
  }

  public static final Factory<E> FACTORY = new Factory<E>() {
    @Override
    public E create() {
      return V1;
    }

    @Override
    public E create(E other) {
      return other;
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test3", "E");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
  };

  /* Json serialization */

  public static JsonBinding<E> jsonBinding() {
    return new JsonBinding<E>() {
      @Override
      public Factory<E> factory() {
        return FACTORY;
      }

      @Override
      public JsonElement toJson(E _value) {
        return new JsonPrimitive(_value.toString());
      }

      @Override
      public E fromJson(JsonElement _json) {
        try {
          return fromString(_json.getAsString());
        } catch (IllegalArgumentException e) {
          throw new JsonParseException(e.getMessage());
        }
      }
    };
  }
}
