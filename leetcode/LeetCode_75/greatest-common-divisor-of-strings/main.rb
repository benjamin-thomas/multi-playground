=begin
 fn test_gcd_of_strings() {
        assert_eq!(
            String::from("ABC"),
            gcd_of_strings(String::from("ABCABC"), String::from("ABC")),
        );
        assert_eq!(
            String::from("AB"),
            gcd_of_strings(String::from("ABABAB"), String::from("ABAB")),
        );
        assert_eq!(
            String::from(""),
            gcd_of_strings(String::from("LEET"), String::from("CODE")),
        );
        assert_eq!(
            String::from(""),
            gcd_of_strings(String::from("ABCDEF"), String::from("ABC")),
        );
        assert_eq!(
            String::from("TAUXX"),
            gcd_of_strings(
                String::from("TAUXXTAUXXTAUXXTAUXXTAUXX"),
                String::from("TAUXXTAUXXTAUXXTAUXXTAUXXTAUXXTAUXXTAUXXTAUXX")
            )
        )
    }
=end


def gcd_of_strings(str1, str2)
  "TODO"
end



# ruby -r minitest/autorun ./main.rb
class Test < Minitest::Test
  # cspell:disable
  def test_gcd_of_strings
    assert_equal("ABC", gcd_of_strings("ABCABC", "ABC"))
    assert_equal("AB", gcd_of_strings("ABABAB", "ABAB"))
    assert_equal("", gcd_of_strings("LEET", "CODE"))
    assert_equal("", gcd_of_strings("ABCDEF", "ABC"))
    assert_equal("TAUXX", gcd_of_strings("TAUXXTAUXXTAUXXTAUXXTAUXX", "TAUXXTAUXXTAUXXTAUXXTAUXXTAUXXTAUXXTAUXXTAUXX"))
  end
  # cspell:enable
end
