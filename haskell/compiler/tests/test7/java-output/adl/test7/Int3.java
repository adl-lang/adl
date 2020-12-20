/* @generated from adl module test7 */

package adl.test7;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.sys.adlast.ScopedName;
import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.sys.adlast.TypeRef;
import java.util.ArrayList;
import java.util.Objects;

public class Int3 {

  /* Members */

  private long value;

  /* Constructors */

  public Int3(long value) {
    this.value = value;
  }

  public Int3() {
    this.value = 42L;
  }

  public Int3(Int3 other) {
    this.value = other.value;
  }

  /* Accessors and mutators */

  public long getValue() {
    return value;
  }

  public void setValue(long value) {
    this.value = value;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof Int3)) {
      return false;
    }
    Int3 other = (Int3) other0;
    return
      value == other.value;
  }

  @Override
  public int hashCode() {
    int _result = 1;
    _result = _result * 37 + (int) (value ^ (value >>> 32));
    return _result;
  }

  /* Factory for construction of generic values */

  public static final Factory<Int3> FACTORY = new Factory<Int3>() {
    @Override
    public Int3 create(Int3 other) {
      return new Int3(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test7", "Int3");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
    @Override
    public JsonBinding<Int3> jsonBinding() {
      return Int3.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<Int3> jsonBinding() {
    final JsonBinding<Long> _binding = JsonBindings.INT64;
    final Factory<Int3> _factory = FACTORY;

    return new JsonBinding<Int3>() {
      @Override
      public Factory<Int3> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(Int3 _value) {
        return _binding.toJson(_value.value);
      }

      @Override
      public Int3 fromJson(JsonElement _json) {
        return new Int3(_binding.fromJson(_json));
      }
    };
  }
}
