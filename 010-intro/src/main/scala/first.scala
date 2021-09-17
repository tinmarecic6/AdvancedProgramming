object First{
    def main(args: Array[String]):Unit = {
        f(20)    
    }
    def f(n: Int) : Int={
        println(n*2)
        if(n>0){
            f(n-1)
            n
        }
        else n
    }

}