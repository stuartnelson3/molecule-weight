$(document).on("click", "button", function(e) {
  var data = {
    peptide_sequence: $("input[name=peptide_sequence]").val(),
    weight: parseFloat($("input[name=weight]").val())
  };

  params = {
    type: 'POST',
    data: data,
    dataType: 'text',
    url: '/possible-matches',
    success: function(data) {
      data = JSON.parse(data)['possible_sequences'];
      var sequences = Object.keys(data);
      for (var i = 0; i < sequences.length; i++) {
        var sequence = sequences[i]
        var weight = data[sequence]
        $(".result table").append("<tr><td>"+sequence+"</td><td>"+weight+"</td></tr>")
      }
    }
  }

  $.ajax(params);
});
