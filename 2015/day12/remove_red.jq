walk (
  if (type=="object") and
     (to_entries | map(select(.value == "red"))) != []
    then
      "removed"
    else
      .
    end
)
