import java.io.*;

public class Main {
    final static String TAB = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;";

    public static void main(String[] args) {
        String html = "<div><h1>My SyntaxHighlighter Test</h1><p>This is a sample of code for " + "my Scanner</p></div>";
        String html2 = "<!DOCTYPE html>\n" +
                "<html>\n" +
                "<head>\n" +
                "    <title>MyOutputTest.cpp</title>\n" +
                "</head>\n" +
                "<body>";
        File f = new File("test.html");
        try {
            BufferedWriter bw = new BufferedWriter(new FileWriter(f));
            FileReader fr = new FileReader("src/input.txt");
            MyScanner Sc = new MyScanner(fr);
            bw.write(html);
            bw.write(html2);
            for (Symbol input = Sc.next(); input.tokenType != TokenType.EOF; input = Sc.next()) {
                TokenType token = input.tokenType;
                if (token == TokenType.ENTER)
                    bw.write("<br>");
                else if (token == TokenType.TAB)
                    bw.write(TAB);
                else if (token == TokenType.LESSTHAN)
                    bw.write("<");
                else if (token == TokenType.MORETHAN)
                    bw.write(">");
                else if (token == TokenType.IDENTIFIER)
                    bw.write("<span style=\"color:violet\">" + input.content + "</span>");
                else if (token == TokenType.INTEGERLITERAL)
                    bw.write("<span style=\"color:orange\">" + input.content + "</span>");
                else if (token == TokenType.STRING || token == TokenType.NORMAL_CHARACTER)
                    bw.write("<span style=\"color:green\">" + input.content + "</span>");
                else if (token == TokenType.SPECIAL_CHARACTER)
                    bw.write("<span style=\"color:green\">" + input.content + "</i></span>");
                else if (token == TokenType.COMMENT)
                    bw.write("<span style=\"color:gray\">" + comment(input.content) + "</span>");
                else if (token == TokenType.FLOATLITERAL)
                    bw.write("<span style=\"color:orange\"><i>" + input.content + "</i></span>");
                else if (token != TokenType.OTHER)
                    bw.write("<span style=\"color:black\"><b>" + input.content + "</b></span>");
                else
                    bw.write(input.content);
            }
            bw.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static String comment(String s) {
        String a = "";
        for (int i = 0; i < s.length(); i++) {
            if (s.charAt(i) == '\n')
                a = a.concat("<br>");
            else if (s.charAt(i) == '\t')
                a = a.concat(TAB);
            else
                a = a.concat(Character.toString(s.charAt(i)));
        }
        return a;
    }
}