$(document).on("click", ".search", postData);
$(document).on("keypress", function(e) {
  if (e.keyCode === 13) {
    postData();
  }
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
  var data = {
    peptide_sequence: $("input[name=peptide_sequence]").val(),
    weight: enteredWeight
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

      var sequences = Object.keys(seq_objs);
      for (var i = 0; i < sequences.length; i++) {
        var sequence = sequences[i];
        var weight = seq_objs[sequence];
        $(".result table").
          append("<tr class="+weightSimilarity(enteredWeight, weight)+">\
                   <td class='found-sequence'>"+sequence+"</td><td>"+weight+"</td></tr>");
      }
    }
  }

  $.ajax(params);
};
