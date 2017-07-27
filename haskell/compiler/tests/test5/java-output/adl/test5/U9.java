/* Code generated from adl module test5 */

package adl.test5;

import com.google.gson.JsonElement;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.JsonParseException;
import org.adl.runtime.Lazy;
import org.adl.sys.adlast.ScopedName;
import org.adl.sys.adlast.TypeExpr;
import org.adl.sys.adlast.TypeRef;
import java.util.ArrayList;
import java.util.Objects;

public class U9<T> {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The U9 discriminator type.
   */
  public enum Disc {
    V1,
    V2
  }

  /* Constructors */

  public static <T> U9<T> v1(T v) {
    return new U9<T>(Disc.V1, Objects.requireNonNull(v));
  }

  public static <T> U9<T> v2(short v) {
    return new U9<T>(Disc.V2, v);
  }

  private U9(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  public T getV1() {
    if (disc == Disc.V1) {
      return U9.<T>cast(value);
    }
    throw new IllegalStateException();
  }

  public short getV2() {
    if (disc == Disc.V2) {
      return (Short) value;
    }
    throw new IllegalStateException();
  }

  /* Mutators */

  public void setV1(T v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.V1;
  }

  public void setV2(short v) {
    this.value = v;
    this.disc = Disc.V2;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof U9)) {
      return false;
    }
    U9<?> other = (U9<?>) other0;
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

  public static <T> Factory<U9 <T>> factory(Factory<T> factoryT) {
    return new Factory<U9<T>>() {
      final Lazy<Factory<T>> v1 = new Lazy<>(() -> factoryT);
      final Lazy<Factory<Short>> v2 = new Lazy<>(() -> Factories.INT16);

      @Override
      public U9<T> create() {
        return new U9<T>(Disc.V1,v1.get().create());
      }

      @Override
      public U9<T> create(U9<T> other) {
        switch (other.disc) {
          case V1:
            return new U9<T>(other.disc,v1.get().create(U9.<T>cast(other.value)));
          case V2:
            return new U9<T>(other.disc,other.value);
        }
        throw new IllegalArgumentException();
      }

      @Override
      public TypeExpr typeExpr() {
        ScopedName scopedName = new ScopedName("test5", "U9");
        ArrayList<TypeExpr> params = new ArrayList<>();
        params.add(factoryT.typeExpr());
        return new TypeExpr(TypeRef.reference(scopedName), params);
      }
    };
  }

  /* Json serialization */

  public static<T> JsonBinding<U9<T>> jsonBinding(JsonBinding<T> bindingT) {
    final Lazy<JsonBinding<T>> v1 = new Lazy<>(() -> bindingT);
    final Lazy<JsonBinding<Short>> v2 = new Lazy<>(() -> JsonBindings.INT16);
    final Factory<T> factoryT = bindingT.factory();
    final Factory<U9<T>> _factory = factory(bindingT.factory());

    return new JsonBinding<U9<T>>() {
      @Override
      public Factory<U9<T>> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(U9<T> _value) {
        switch (_value.getDisc()) {
          case V1:
            return JsonBindings.unionToJson("v1", _value.getV1(), v1.get());
          case V2:
            return JsonBindings.unionToJson("v2", _value.getV2(), v2.get());
        }
        return null;
      }

      @Override
      public U9<T> fromJson(JsonElement _json) {
        String _key = JsonBindings.unionNameFromJson(_json);
        if (_key.equals("v1")) {
          return U9.<T>v1(JsonBindings.unionValueFromJson(_json, v1.get()));
        }
        else if (_key.equals("v2")) {
          return U9.<T>v2(JsonBindings.unionValueFromJson(_json, v2.get()));
        }
        throw new JsonParseException("Invalid discriminator " + _key + " for union U9<T>");
      }
    };
  }
}
