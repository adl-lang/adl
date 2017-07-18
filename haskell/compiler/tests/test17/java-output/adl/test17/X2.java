/* Code generated from adl module test17 */

package adl.test17;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.JsonParseException;
import org.adl.runtime.Lazy;
import java.util.ArrayList;
import java.util.Map;
import java.util.Objects;

public class X2 {

  /* Members */

  private Disc disc;
  private Object value;

  /**
   * The X2 discriminator type.
   */
  public enum Disc {
    F1,
    F2,
    F3,
    F4,
    F5,
    F6,
    F7
  }

  /* Constructors */

  public static X2 f1(int v) {
    return new X2(Disc.F1, v);
  }

  public static X2 f2(int v) {
    return new X2(Disc.F2, v);
  }

  public static X2 f3(Pair<String, Integer> v) {
    return new X2(Disc.F3, Objects.requireNonNull(v));
  }

  public static X2 f4(Pair<String, String> v) {
    return new X2(Disc.F4, Objects.requireNonNull(v));
  }

  public static X2 f5(ArrayList<Integer> v) {
    return new X2(Disc.F5, Objects.requireNonNull(v));
  }

  public static X2 f6(ArrayList<Pair<String, Integer>> v) {
    return new X2(Disc.F6, Objects.requireNonNull(v));
  }

  public static X2 f7(ArrayList<Pair<String, String>> v) {
    return new X2(Disc.F7, Objects.requireNonNull(v));
  }

  public X2() {
    this.disc = Disc.F1;
    this.value = 0;
  }

  public X2(X2 other) {
    this.disc = other.disc;
    switch (other.disc) {
      case F1:
        this.value = (Integer) other.value;
        break;
      case F2:
        this.value = (Integer) other.value;
        break;
      case F3:
        this.value = Pair.factory(Factories.STRING, Factories.INTEGER).create(X2.<Pair<String, Integer>>cast(other.value));
        break;
      case F4:
        this.value = Pair.factory(Factories.STRING, Factories.STRING).create(X2.<Pair<String, String>>cast(other.value));
        break;
      case F5:
        this.value = Factories.arrayList(Factories.INTEGER).create(X2.<ArrayList<Integer>>cast(other.value));
        break;
      case F6:
        this.value = Factories.arrayList(Pair.factory(Factories.STRING, Factories.INTEGER)).create(X2.<ArrayList<Pair<String, Integer>>>cast(other.value));
        break;
      case F7:
        this.value = Factories.arrayList(Pair.factory(Factories.STRING, Factories.STRING)).create(X2.<ArrayList<Pair<String, String>>>cast(other.value));
        break;
    }
  }

  private X2(Disc disc, Object value) {
    this.disc = disc;
    this.value = value;
  }

  /* Accessors */

  public Disc getDisc() {
    return disc;
  }

  public int getF1() {
    if (disc == Disc.F1) {
      return (Integer) value;
    }
    throw new IllegalStateException();
  }

  public int getF2() {
    if (disc == Disc.F2) {
      return (Integer) value;
    }
    throw new IllegalStateException();
  }

  public Pair<String, Integer> getF3() {
    if (disc == Disc.F3) {
      return X2.<Pair<String, Integer>>cast(value);
    }
    throw new IllegalStateException();
  }

  public Pair<String, String> getF4() {
    if (disc == Disc.F4) {
      return X2.<Pair<String, String>>cast(value);
    }
    throw new IllegalStateException();
  }

  public ArrayList<Integer> getF5() {
    if (disc == Disc.F5) {
      return X2.<ArrayList<Integer>>cast(value);
    }
    throw new IllegalStateException();
  }

  public ArrayList<Pair<String, Integer>> getF6() {
    if (disc == Disc.F6) {
      return X2.<ArrayList<Pair<String, Integer>>>cast(value);
    }
    throw new IllegalStateException();
  }

  public ArrayList<Pair<String, String>> getF7() {
    if (disc == Disc.F7) {
      return X2.<ArrayList<Pair<String, String>>>cast(value);
    }
    throw new IllegalStateException();
  }

  /* Mutators */

  public void setF1(int v) {
    this.value = v;
    this.disc = Disc.F1;
  }

  public void setF2(int v) {
    this.value = v;
    this.disc = Disc.F2;
  }

  public void setF3(Pair<String, Integer> v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.F3;
  }

  public void setF4(Pair<String, String> v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.F4;
  }

  public void setF5(ArrayList<Integer> v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.F5;
  }

  public void setF6(ArrayList<Pair<String, Integer>> v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.F6;
  }

  public void setF7(ArrayList<Pair<String, String>> v) {
    this.value = Objects.requireNonNull(v);
    this.disc = Disc.F7;
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof X2)) {
      return false;
    }
    X2 other = (X2) other0;
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

  public static final Factory<X2> FACTORY = new Factory<X2>() {
    @Override
    public X2 create() {
      return new X2();
    }

    @Override
    public X2 create(X2 other) {
      return new X2(other);
    }
  };

  /* Json serialization */

  public static JsonBinding<X2> jsonBinding() {
    final Lazy<JsonBinding<Integer>> f1 = new Lazy<>(() -> JsonBindings.INTEGER);
    final Lazy<JsonBinding<Integer>> f2 = new Lazy<>(() -> JsonBindings.INTEGER);
    final Lazy<JsonBinding<Pair<String, Integer>>> f3 = new Lazy<>(() -> Pair.jsonBinding(JsonBindings.STRING, JsonBindings.INTEGER));
    final Lazy<JsonBinding<Pair<String, String>>> f4 = new Lazy<>(() -> Pair.jsonBinding(JsonBindings.STRING, JsonBindings.STRING));
    final Lazy<JsonBinding<ArrayList<Integer>>> f5 = new Lazy<>(() -> JsonBindings.arrayList(JsonBindings.INTEGER));
    final Lazy<JsonBinding<ArrayList<Pair<String, Integer>>>> f6 = new Lazy<>(() -> JsonBindings.arrayList(Pair.jsonBinding(JsonBindings.STRING, JsonBindings.INTEGER)));
    final Lazy<JsonBinding<ArrayList<Pair<String, String>>>> f7 = new Lazy<>(() -> JsonBindings.arrayList(Pair.jsonBinding(JsonBindings.STRING, JsonBindings.STRING)));
    final Factory<X2> _factory = FACTORY;

    return new JsonBinding<X2>() {
      public Factory<X2> factory() {
        return _factory;
      }

      @Override
      public JsonElement toJson(X2 _value) {
        switch (_value.getDisc()) {
          case F1:
            return JsonBindings.unionToJson("f1", _value.getF1(), f1.get());
          case F2:
            return JsonBindings.unionToJson("f2", _value.getF2(), f2.get());
          case F3:
            return JsonBindings.unionToJson("f3", _value.getF3(), f3.get());
          case F4:
            return JsonBindings.unionToJson("f4", _value.getF4(), f4.get());
          case F5:
            return JsonBindings.unionToJson("f5", _value.getF5(), f5.get());
          case F6:
            return JsonBindings.unionToJson("f6", _value.getF6(), f6.get());
          case F7:
            return JsonBindings.unionToJson("f7", _value.getF7(), f7.get());
        }
        return null;
      }

      @Override
      public X2 fromJson(JsonElement _json) {
        String _key = JsonBindings.unionNameFromJson(_json);
        if (_key.equals("f1")) {
          return X2.f1(JsonBindings.unionValueFromJson(_json, f1.get()));
        }
        else if (_key.equals("f2")) {
          return X2.f2(JsonBindings.unionValueFromJson(_json, f2.get()));
        }
        else if (_key.equals("f3")) {
          return X2.f3(JsonBindings.unionValueFromJson(_json, f3.get()));
        }
        else if (_key.equals("f4")) {
          return X2.f4(JsonBindings.unionValueFromJson(_json, f4.get()));
        }
        else if (_key.equals("f5")) {
          return X2.f5(JsonBindings.unionValueFromJson(_json, f5.get()));
        }
        else if (_key.equals("f6")) {
          return X2.f6(JsonBindings.unionValueFromJson(_json, f6.get()));
        }
        else if (_key.equals("f7")) {
          return X2.f7(JsonBindings.unionValueFromJson(_json, f7.get()));
        }
        throw new JsonParseException("Invalid discriminator " + _key + " for union X2");
      }
    };
  }
}
