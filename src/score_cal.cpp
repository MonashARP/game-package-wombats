#include <Rcpp.h>
using namespace Rcpp;

// Convert rank character to numeric value
int rank_to_value(const std::string& rank) {
  if (rank == "J" || rank == "Q" || rank == "K") {
    return 10;
  } else if (rank == "A") {
    return 11;
  } else {
    return std::stoi(rank);
  }
}

//' Calculate Blackjack Score (C++)
//'
//' Fast C++ implementation for calculating blackjack hand scores
//' @name calculate_score_cpp
//' @param ranks Character vector of card ranks (e.g., c("A", "10", "K"))
//' @return Integer score of the hand
//' @export
// [[Rcpp::export]]
int calculate_score_cpp(CharacterVector ranks) {
 int total = 0;
 int ace_count = 0;

 // Calculate initial total and count aces
 for (int i = 0; i < ranks.size(); i++) {
   std::string rank = as<std::string>(ranks[i]);
   int value = rank_to_value(rank);
   total += value;

   if (rank == "A") {
     ace_count++;     }
   }

  // Adjust for aces if total > 21
  while (total > 21 && ace_count > 0) {
   total -= 10;
   ace_count--;
  }

  return total;
}
