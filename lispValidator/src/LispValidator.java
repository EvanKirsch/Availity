import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;
import java.util.Stack;

public class LispValidator {

  public static void main(String[] args) {
    String expression;
    if (args[0].equals("-f")) {
      expression = readFile(args[1]);
    } else {
      expression = args[0];
    }
    System.out.println(validate(expression));
  }

  public static boolean validate(String lispExpression) {
    Stack<Character> stack = new Stack<>();
    for (int i = 0; i < lispExpression.length(); i++) {
      if (lispExpression.charAt(i) == '(') {
        stack.add('(');
      }
      if (lispExpression.charAt(i) == ')') {
        if (!stack.isEmpty() && stack.get(stack.size() - 1) == '(') {
          stack.pop();
        } else {
          stack.add(')');
        }
      }
    }
    return stack.size() == 0;
  }

  private static String readFile(String path) {
    File file = new File(path);
    Scanner scanner;
    try {
      scanner = new Scanner(file);
    } catch (FileNotFoundException e) {
      e.printStackTrace();
      return "";
    }
    StringBuilder stringBuilder = new StringBuilder();
    while (scanner.hasNext()) {
      stringBuilder.append(scanner.next());
    }
    return stringBuilder.toString();
  }

}