$(document).on("click", "button", postData);
$(document).on("keypress", function(e) {
  if (e.keyCode === 13) {
    postData();
  }
});

function postData(e) {
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
      data = JSON.parse(data)
      seq_objs = data['possible_sequences'];
      if (typeof seq_objs === "string") {
        $(".result table").append("<tr><td>"+data+"</td></tr>")
        return
      }
      var sequences = Object.keys(seq_objs);
      for (var i = 0; i < sequences.length; i++) {
        var sequence = sequences[i]
        var weight = seq_objs[sequence]
        $(".result table").append("<tr><td>"+sequence+"</td><td>"+weight+"</td></tr>")
      }
      $(".peptide-weight").append("<p>Peptide weight is "+data['weight']+"</p>");
    }
  }

  $.ajax(params);
};
