package adl.test;

public class S3<T> {

  public String f1;
  public double f2;
  public T f3;
  public java.util.ArrayList<T> f4;

  public S3(String f1, double f2, T f3, java.util.ArrayList<T> f4) {
    this.f1 = f1;
    this.f2 = f2;
    this.f3 = f3;
    this.f4 = f4;
  }

  public S3(Class<T> classT) {
    try {
      this.f1 = "";
      this.f2 = 0;
      this.f3 = classT.getDeclaredConstructor().newInstance();
      this.f4 = new java.util.ArrayList<T>();
    }
    catch (ReflectiveOperationException e) {
      throw new RuntimeException(e);
    }
  }

  public S3(S3<T> other, Class<T> classT) {
    try {
      this.f1 = other.f1;
      this.f2 = other.f2;
      this.f3 = classT.getDeclaredConstructor(other.f3.getClass()).newInstance(other.f3);
      this.f4 = new java.util.ArrayList<T>(other.f4);
    }
    catch (ReflectiveOperationException e) {
      throw new RuntimeException(e);
    }
  }
}