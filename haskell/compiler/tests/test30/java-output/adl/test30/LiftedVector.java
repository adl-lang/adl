/* @generated from adl module test30 */

package adl.test30;

import com.google.gson.JsonElement;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.JsonParseException;
import org.adl.runtime.Lazy;
import org.adl.runtime.sys.adlast.ScopedName;
import org.adl.runtime.sys.adlast.TypeExpr;
import org.adl.runtime.sys.adlast.TypeRef;
import org.adl.runtime.sys.types.Pair;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class LiftedVector {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The LiftedVector discriminator type.
   */
  public enum Disc {
    STR_ARR,
    COUNT_STRS
  }

  /* Constructors */

  public static LiftedVector str_arr(List<String> v) {
    return new LiftedVector(Disc.STR_ARR, Objects.requireNonNull(v));
  }

  public static LiftedVector count_strs(Pair<Long, List<String>> v) {
    return new LiftedVector(Disc.COUNT_STRS, Objects.requireNonNull(v));
  }

  public LiftedVector(LiftedVector other) {
    this.disc = other.disc;
    switch (other.disc) {
      case STR_ARR:
        this.value = Factories.list(Factories.STRING).create(LiftedVector.<List<String>>cast(other.value));
        break;
      case COUNT_STRS:
        this.value = Pair.factory(Factories.INT64, Factories.list(Factories.STRING)).create(LiftedVector.<Pair<Long, List<String>>>cast(other.value));
        break;
    }
  }

  private LiftedVector(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  public List<String> getStr_arr() {
    if (disc == Disc.STR_ARR) {
      return LiftedVector.<List<String>>cast(value);
    }
    throw new IllegalStateException();
  }

  public Pair<Long, List<String>> getCount_strs() {
    if (disc == Disc.COUNT_STRS) {
      return LiftedVector.<Pair<Long, List<String>>>cast(value);
    }
    throw new IllegalStateException();
  }

  /* Mutators */

  public void setStr_arr(List<String> v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.STR_ARR;
  }

  public void setCount_strs(Pair<Long, List<String>> v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.COUNT_STRS;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof LiftedVector)) {
      return false;
    }
    LiftedVector other = (LiftedVector) other0;
    return disc == other.disc && value.equals(other.value);
  }

  @Override
  public int hashCode() {
    return disc.hashCode() * 37 + value.hashCode();
  }

  @SuppressWarnings("unchecked")
  private static <T> T cast(final Object o) {
    return (T) o;
  }

  /* Factory for construction of generic values */

  public static final Factory<LiftedVector> FACTORY = new Factory<LiftedVector>() {
    @Override
    public LiftedVector create(LiftedVector other) {
      return new LiftedVector(other);
    }

    @Override
    public TypeExpr typeExpr() {
      ScopedName scopedName = new ScopedName("test30", "LiftedVector");
      ArrayList<TypeExpr> params = new ArrayList<>();
      return new TypeExpr(TypeRef.reference(scopedName), params);
    }

    @Override
    public JsonBinding<LiftedVector> jsonBinding() {
      return LiftedVector.jsonBinding();
    }
  };

  /* Json serialization */

  public static JsonBinding<LiftedVector> jsonBinding() {
    final Lazy<JsonBinding<List<String>>> str_arr = new Lazy<>(() -> JsonBindings.list(JsonBindings.STRING));
    final Lazy<JsonBinding<Pair<Long, List<String>>>> count_strs = new Lazy<>(() -> Pair.jsonBinding(JsonBindings.INT64, JsonBindings.list(JsonBindings.STRING)));
    final Factory<LiftedVector> _factory = FACTORY;

    return new JsonBinding<LiftedVector>() {
      @Override
      public Factory<LiftedVector> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(LiftedVector _value) {
        switch (_value.getDisc()) {
          case STR_ARR:
            return JsonBindings.unionToJson("str_arr", _value.getStr_arr(), str_arr.get());
          case COUNT_STRS:
            return JsonBindings.unionToJson("count_strs", _value.getCount_strs(), count_strs.get());
        }
        return null;
      }

      private LiftedVector fromJsonUnion(JsonElement _json) {
        String _key = JsonBindings.unionNameFromJson(_json);
        if (_key.equals("str_arr")) {
          return LiftedVector.str_arr(JsonBindings.unionValueFromJson(_json, str_arr.get()));
        }
        else if (_key.equals("count_strs")) {
          return LiftedVector.count_strs(JsonBindings.unionValueFromJson(_json, count_strs.get()));
        }
        throw new JsonParseException("Invalid discriminator " + _key + " for union LiftedVector");
      }

      @Override
      public LiftedVector fromJson(JsonElement _json) {
        try {
          return fromJsonUnion(_json);
        } catch (Exception e) {
          try {
            _json.getAsString();
          } catch (UnsupportedOperationException | ClassCastException e0) {
            try {
              return LiftedVector.str_arr(str_arr.get().fromJson(_json));
            } catch(JsonParseException e2) {
              throw e;
            }
          }
          throw new JsonParseException( "can't lift String or Void using AllowUntaggedDeserializeOfFirstBranch");
        }
      }
    };
  }
}
