/* Code generated from adl module test5 */

package adl.test5;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import org.adl.sys.adlast.ScopedName;
import org.adl.sys.adlast.TypeExpr;
import org.adl.sys.adlast.TypeRef;
import java.util.ArrayList;
import java.util.Objects;

public class S1 {

  /* Members */

  private short f;

  /* Constructors */

  public S1(short f) {
    this.f = f;
  }

  public S1() {
    this.f = (short)100;
  }

  public S1(S1 other) {
    this.f = other.f;
  }

  /* Accessors and mutators */

  public short getF() {
    return f;
  }

  public void setF(short f) {
    this.f = f;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof S1)) {
      return false;
    }
    S1 other = (S1) other0;
    return
      f == other.f;
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + (int) f;
    return _result;
  }

  /* Factory for construction of generic values */

  public static final Factory<S1> FACTORY = new Factory<S1>() {
    @Override
    public S1 create() {
      return new S1();
    }

    @Override
    public S1 create(S1 other) {
      return new S1(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test5", "S1");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
  };

  /* Json serialization */

  public static JsonBinding<S1> jsonBinding() {
    final Lazy<JsonBinding<Short>> f = new Lazy<>(() -> JsonBindings.INT16);
    final Factory<S1> _factory = FACTORY;

    return new JsonBinding<S1>() {
      @Override
      public Factory<S1> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(S1 _value) {
        JsonObject _result = new JsonObject();
        _result.add("f", f.get().toJson(_value.f));
        return _result;
      }

      @Override
      public S1 fromJson(JsonElement _json) {
        JsonObject _obj = JsonBindings.objectFromJson(_json);
        return new S1(
          _obj.has("f") ? JsonBindings.fieldFromJson(_obj, "f", f.get()) : (short)100
        );
      }
    };
  }
}
