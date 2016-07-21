package adl.test;

import org.adl.runtime.Factories;
import org.adl.runtime.Factory;

public class S1 {

  /* Members */

  private int x;
  private String y;

  /* Constructors */

  public S1(int x, String y) {
    this.x = x;
    this.y = java.util.Objects.requireNonNull(y);
  }

  public S1() {
    this.x = 0;
    this.y = "";
  }

  public S1(S1 other) {
    this.x = other.x;
    this.y = other.y;
  }

  /* Accessors and mutators */

  public int getX() {
    return x;
  }

  public void setX(int newX) {
    x = newX;
  }

  public String getY() {
    return y;
  }

  public void setY(String newY) {
    y = newY;
  }

  /* Object level helpers */

  public boolean equals(S1 other) {
    return
      x == other.x &&
      y.equals(other.y);
  }

  public int hashCode() {
    int result = 1;
    result = result * 37 + x;
    result = result * 37 + y.hashCode();
    return result;
  }

  /* Factory for construction of generic values */

  public static Factory<S1> factory = new Factory<S1>() {
    public S1 create() {
      return new S1();
    }
    public S1 create(S1 other) {
      return new S1(other);
    }
  };
}