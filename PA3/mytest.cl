class Main inherits IO {


	numA:Int <- 0;


	main(): SELF_TYPE {
		{
			-- (new IO)@IO.out_string(1, 2, 3);
			io@IO.out_string(1, 2, 3);
			funB();
			a@C.funC();
			1;
			let a:Int, b:String in {
				a;
			};
		}
	};

};

class Dummy {

	attrA:Int <- 0;

	dummy(): SELF_TYPE {
		{
			case a of
				varA : Int => 1;
				varB : Int => 2;
				varC : Int => 3;
				varD : Int => 4;
				varE : Int => 5;
			esac;

			let a:Int, b:String in {
				a;
			};

			a + b;
			a - b;
			a * b;
			a / b;
			~a;
			a < b;
			a <= b;
			a = b;
			not a;

			a + b * c;
			b * c + a;
			b * (c + a);
			(a + b) * c;
		}
	};
};

class Simple {
	method1(): SELF_TYPE {
		{
			a@Simple.method1(1, 2, 3);
			method1();
			if a then b else c fi;
			1 < 2 < 3; }
	};

};

class ErrorFeatures {
	AttrA:Int;

	method1(): SELF_TYPE {
	};

	method2(): GOOD {
		1
	};

	method3(): BLOCK_ERR {
		{
			1;
			2
		}
	};

	method4(): LET_ERR {
		let a:Int, b:int, c:Object in {
			1;
		}
	};

	method5(): A {
		1
	}
};
