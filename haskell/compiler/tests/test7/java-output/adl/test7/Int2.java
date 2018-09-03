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

public class Int2 {

  /* Members */

  private long value;

  /* Constructors */

  public Int2(long value) {
    this.value = value;
  }

  public Int2() {
    this.value = 0L;
  }

  public Int2(Int2 other) {
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
    if (!(other0 instanceof Int2)) {
      return false;
    }
    Int2 other = (Int2) other0;
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

  public static final Factory<Int2> FACTORY = new Factory<Int2>() {
    @Override
    public Int2 create() {
      return new Int2();
    }

    @Override
    public Int2 create(Int2 other) {
      return new Int2(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test7", "Int2");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }
  };

  /* Json serialization */

  public static JsonBinding<Int2> jsonBinding() {
    final JsonBinding<Long> _binding = JsonBindings.INT64;
    final Factory<Int2> _factory = FACTORY;

    return new JsonBinding<Int2>() {
      @Override
      public Factory<Int2> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(Int2 _value) {
        return _binding.toJson(_value.value);
      }

      @Override
      public Int2 fromJson(JsonElement _json) {
        return new Int2(_binding.fromJson(_json));
      }
    };
  }
}
