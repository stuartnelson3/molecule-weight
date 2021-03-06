$(document).on("click", ".search", postData);
$(document).on("keypress", function(e) {
  if (e.keyCode === 13) {
    postData();
  }
});

$(document).on('input', '.js-range-input', function(e) {
  $('.js-range-value').html(e.currentTarget.value);
});

function weightSimilarity(enteredWeight, weight) {
  var absoluteDiff = Math.abs(enteredWeight - weight);
  if (absoluteDiff <= 1.5) {
    return "green"
  }
  else if (absoluteDiff <= 3) {
    return "yellow"
  }
  else {
    return "red"
  }
};

$(document).on("click", ".clear-results", function() {
  $(".red,.yellow,.green, .error").remove();
  $(".peptide-weight").empty();
});

function postData(e) {
  var enteredWeight = parseFloat($("input[name=weight]").val());
  var wildcards = {
    "(1X)": $("input[name='(1X)']").val(),
    "(2X)": $("input[name='(2X)']").val(),
    "(3X)": $("input[name='(3X)']").val(),
  }
  var mass_type = $(".mass-select select").val();
  var enteredSequence = $("input[name=peptide_sequence]").val();
  var tolerance = $(".js-range-input").val();

  var data = {
    peptide_sequence: enteredSequence,
    weight: enteredWeight,
    wildcards: wildcards,
    mass_type: mass_type,
    tolerance: tolerance,
  };

  var params = {
    type: 'POST',
    data: data,
    url: '/possible-matches',
    success: function(data) {
      data = JSON.parse(data);
      var seq_objs = data['possible_sequences'];

      $(".peptide-weight").empty().append("<p>Expected [M+H]<span class='superscript'>+</span> is "+data['weight']+"</p>");
      if (typeof seq_objs === "string") {
        $(".result table").append("<tr><td class='error'>"+seq_objs+"</td></tr>");
        return;
      }

      // Sort closest value to found mass at top.
      var sequences = []
      for (var k in seq_objs) {
        sequences.push({'sequence': k, 'weight': seq_objs[k]});
      }
      sequences = sequences.sort(function(a, b) {
        return Math.abs(a.weight - enteredWeight) - Math.abs(b.weight - enteredWeight);
      });
      $(".result table").empty();
      sequences.forEach(function(match) {
        var sequence = match.sequence;
        var weight = match.weight;
        var regex = /no matches found/i;
        var color = (regex.test(sequence) && 'red') || weightSimilarity(enteredWeight, weight);
        $(".result table").
          append("<tr class="+color+">\
                   <td class='found-sequence'>"+sequence+"</td><td>"+weight+"</td></tr>");
      });
    }
  }

  $.ajax(params);
};
