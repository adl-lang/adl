/* Code generated from adl module test17 */

package adl.test17;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.adl.runtime.Factories;
import org.adl.runtime.Factory;
import org.adl.runtime.JsonBinding;
import org.adl.runtime.JsonBindings;
import org.adl.runtime.Lazy;
import java.util.ArrayList;
import java.util.Objects;

public class X1 {

  /* Members */

  private int f1;
  private int f2;
  private Pair<String, Integer> f3;
  private Pair<String, String> f4;
  private ArrayList<Integer> f5;
  private ArrayList<Pair<String, Integer>> f6;
  private ArrayList<Pair<String, String>> f7;

  /* Constructors */

  public X1(int f1, int f2, Pair<String, Integer> f3, Pair<String, String> f4, ArrayList<Integer> f5, ArrayList<Pair<String, Integer>> f6, ArrayList<Pair<String, String>> f7) {
    this.f1 = f1;
    this.f2 = f2;
    this.f3 = Objects.requireNonNull(f3);
    this.f4 = Objects.requireNonNull(f4);
    this.f5 = Objects.requireNonNull(f5);
    this.f6 = Objects.requireNonNull(f6);
    this.f7 = Objects.requireNonNull(f7);
  }

  public X1() {
    this.f1 = 0;
    this.f2 = 0;
    this.f3 = Pair.factory(Factories.STRING, Factories.INTEGER).create();
    this.f4 = Pair.factory(Factories.STRING, Factories.STRING).create();
    this.f5 = new ArrayList<Integer>();
    this.f6 = new ArrayList<Pair<String, Integer>>();
    this.f7 = new ArrayList<Pair<String, String>>();
  }

  public X1(X1 other) {
    this.f1 = other.f1;
    this.f2 = other.f2;
    this.f3 = Pair.factory(Factories.STRING, Factories.INTEGER).create(other.f3);
    this.f4 = Pair.factory(Factories.STRING, Factories.STRING).create(other.f4);
    this.f5 = Factories.arrayList(Factories.INTEGER).create(other.f5);
    this.f6 = Factories.arrayList(Pair.factory(Factories.STRING, Factories.INTEGER)).create(other.f6);
    this.f7 = Factories.arrayList(Pair.factory(Factories.STRING, Factories.STRING)).create(other.f7);
  }

  /* Accessors and mutators */

  public int getF1() {
    return f1;
  }

  public void setF1(int f1) {
    this.f1 = f1;
  }

  public int getF2() {
    return f2;
  }

  public void setF2(int f2) {
    this.f2 = f2;
  }

  public Pair<String, Integer> getF3() {
    return f3;
  }

  public void setF3(Pair<String, Integer> f3) {
    this.f3 = Objects.requireNonNull(f3);
  }

  public Pair<String, String> getF4() {
    return f4;
  }

  public void setF4(Pair<String, String> f4) {
    this.f4 = Objects.requireNonNull(f4);
  }

  public ArrayList<Integer> getF5() {
    return f5;
  }

  public void setF5(ArrayList<Integer> f5) {
    this.f5 = Objects.requireNonNull(f5);
  }

  public ArrayList<Pair<String, Integer>> getF6() {
    return f6;
  }

  public void setF6(ArrayList<Pair<String, Integer>> f6) {
    this.f6 = Objects.requireNonNull(f6);
  }

  public ArrayList<Pair<String, String>> getF7() {
    return f7;
  }

  public void setF7(ArrayList<Pair<String, String>> f7) {
    this.f7 = Objects.requireNonNull(f7);
  }

  /* Object level helpers */

  @Override
  public boolean equals(Object other0) {
    if (!(other0 instanceof X1)) {
      return false;
    }
    X1 other = (X1) other0;
    return
      f1 == other.f1 &&
      f2 == other.f2 &&
      f3.equals(other.f3) &&
      f4.equals(other.f4) &&
      f5.equals(other.f5) &&
      f6.equals(other.f6) &&
      f7.equals(other.f7);
  }

  @Override
  public int hashCode() {
    int result = 1;
    result = result * 37 + f1;
    result = result * 37 + f2;
    result = result * 37 + f3.hashCode();
    result = result * 37 + f4.hashCode();
    result = result * 37 + f5.hashCode();
    result = result * 37 + f6.hashCode();
    result = result * 37 + f7.hashCode();
    return result;
  }

  /* Factory for construction of generic values */

  public static final Factory<X1> FACTORY = new Factory<X1>() {
    public X1 create() {
      return new X1();
    }
    public X1 create(X1 other) {
      return new X1(other);
    }
  };

  /* Json serialization */

  public static JsonBinding<X1> jsonBinding() {
    final Lazy<JsonBinding<Integer>> f1 = new Lazy<>(() -> JsonBindings.INTEGER);
    final Lazy<JsonBinding<Integer>> f2 = new Lazy<>(() -> JsonBindings.INTEGER);
    final Lazy<JsonBinding<Pair<String, Integer>>> f3 = new Lazy<>(() -> Pair.jsonBinding(JsonBindings.STRING, JsonBindings.INTEGER));
    final Lazy<JsonBinding<Pair<String, String>>> f4 = new Lazy<>(() -> Pair.jsonBinding(JsonBindings.STRING, JsonBindings.STRING));
    final Lazy<JsonBinding<ArrayList<Integer>>> f5 = new Lazy<>(() -> JsonBindings.arrayList(JsonBindings.INTEGER));
    final Lazy<JsonBinding<ArrayList<Pair<String, Integer>>>> f6 = new Lazy<>(() -> JsonBindings.arrayList(Pair.jsonBinding(JsonBindings.STRING, JsonBindings.INTEGER)));
    final Lazy<JsonBinding<ArrayList<Pair<String, String>>>> f7 = new Lazy<>(() -> JsonBindings.arrayList(Pair.jsonBinding(JsonBindings.STRING, JsonBindings.STRING)));
    final Factory<X1> _factory = FACTORY;

    return new JsonBinding<X1>() {
      public Factory<X1> factory() {
        return _factory;
      }

      public JsonElement toJson(X1 _value) {
        JsonObject _result = new JsonObject();
        _result.add("f1", f1.get().toJson(_value.f1));
        _result.add("f2", f2.get().toJson(_value.f2));
        _result.add("f3", f3.get().toJson(_value.f3));
        _result.add("f4", f4.get().toJson(_value.f4));
        _result.add("f5", f5.get().toJson(_value.f5));
        _result.add("f6", f6.get().toJson(_value.f6));
        _result.add("f7", f7.get().toJson(_value.f7));
        return _result;
      }

      public X1 fromJson(JsonElement _json) {
        JsonObject _obj = _json.getAsJsonObject();
        return new X1(
          _obj.has("f1") ? f1.get().fromJson(_obj.get("f1")) : 0,
          _obj.has("f2") ? f2.get().fromJson(_obj.get("f2")) : 0,
          _obj.has("f3") ? f3.get().fromJson(_obj.get("f3")) : Pair.factory(Factories.STRING, Factories.INTEGER).create(),
          _obj.has("f4") ? f4.get().fromJson(_obj.get("f4")) : Pair.factory(Factories.STRING, Factories.STRING).create(),
          _obj.has("f5") ? f5.get().fromJson(_obj.get("f5")) : new ArrayList<Integer>(),
          _obj.has("f6") ? f6.get().fromJson(_obj.get("f6")) : new ArrayList<Pair<String, Integer>>(),
          _obj.has("f7") ? f7.get().fromJson(_obj.get("f7")) : new ArrayList<Pair<String, String>>()
        );
      }
    };
  }
}
