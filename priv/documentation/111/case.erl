good(Animal) ->
    case Animal of
        kitty ->
            boo;
        doggy ->
            yay;
        _Else ->
            scary
    end.


bad(Animal) ->
   case Animal of
        kitty ->
          boo;
     dog ->
         yay;
      _Else ->
          scary
  end.
