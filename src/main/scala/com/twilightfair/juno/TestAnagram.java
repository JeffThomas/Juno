package com.twilightfair.juno;

/**
 * Created by jthomas on 1/29/14.
 */
    public class TestAnagram {
      public static boolean isAnagram(String first, String second) {
        String positive = first.toLowerCase();
        String negative = second.toLowerCase();

        if (positive.length() != negative.length()) {
          return false;
        }

        int[] counts = new int[26];

        int diff = 0;

        for (int i = 0; i < positive.length(); i++) {
          int pos = (int) positive.charAt(i) - 97; // convert the char into an array index
          if (counts[pos] >= 0) { // the other string doesn't have this
            diff++; // an increase in differences
          } else {
            diff--; // a decrease in differences
          }
          counts[pos]++; // track it

          int neg = (int) negative.charAt(i) - 97;
          if (counts[neg] <= 0) { // the other string doesn't have this
            diff++; // an increase in differences
          } else {
            diff--; // a decrease in differences
          }
          counts[neg]--; // track it
        }

        return diff == 0;
      }

      public static void main(String[] args) {
        System.out.println(isAnagram("zMarry", "zArmry")); // true
        System.out.println(isAnagram("basiparachromatin", "marsipobranchiata")); // true
        System.out.println(isAnagram("hydroxydeoxycorticosterones", "hydroxydesoxycorticosterone")); // true
        System.out.println(isAnagram("hydroxydeoxycorticosterones", "hydroxydesoxycorticosterons")); // false
        System.out.println(isAnagram("zArmcy", "zArmry")); // false
      }
    }
