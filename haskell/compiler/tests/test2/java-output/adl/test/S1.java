package adl.test;

public class S1 {

  private int x;
  private String y;

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

  public boolean equals(S1 other) {
    return
      x == other.x &&
      y.equals(other.y);
  }

  public int hashCode() {
    int result = 1;
    result = result * 37 + (int)x;
    result = result * 37 + y.hashCode();
    return result;
  }
}