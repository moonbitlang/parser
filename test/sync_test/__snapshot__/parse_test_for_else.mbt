fn init {
  for ; ; {
    break(Some(0))
  } nobreak {
    None
  }

  for ; ; {
    if true {
      println(3)
    } else {
      println(4)
    }
  } nobreak {
    println(5)
  }

  for ; ; {
    if true {
      println(3)
    } else {
      println(4)
    }
  } else if(test) {
    println(5)
  }

}

fn init {
  let a = (for ; ; {  })
  let b = a + (for ; ; {  })
  return for ; ; {  }
}

fn init {
  for i = (for ; ; {  }); i < (for ; ; {  }); {
    break(for ; ; {  })
  }
}
