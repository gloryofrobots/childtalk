package ua.ho.gloryofrobots.childtalk.consoleapp;

import java.util.ArrayList;
import java.util.List;

import ua.ho.gloryofrobots.childtalk.bootstrap.ImageSuite;
import ua.ho.gloryofrobots.childtalk.stobject.STClass;
import ua.ho.gloryofrobots.childtalk.stobject.STMethod;
import ua.ho.gloryofrobots.childtalk.stobject.STObject;
import ua.ho.gloryofrobots.childtalk.stobject.STScope;
import ua.ho.gloryofrobots.childtalk.stobject.STSymbol;

//DUMB class just for testing unimplemented primitives/

class PrimitiveTester {
    public void _getUnknownPrimitives(List<STSymbol> primitives, STScope scope) {
        List<STObject> objects = scope.asList();
        for (STObject obj : objects) {
            if ((obj instanceof STMethod) == false) {
                continue;
            }

            STMethod method = (STMethod) obj;
            if (method.hasPrimitive() == false) {
                continue;
            }

            if (method.getPrimitive() == null) {
                method.getPrimitive();

                if (primitives.contains(method.getPrimitiveName())) {
                    continue;
                }

                primitives.add(method.getPrimitiveName());
            }
        }
    }

    public List<STSymbol> getUnknownPrimitives(STClass klass,
            List<STSymbol> primitives) {
        _getUnknownPrimitives(primitives, klass.getScope());
        STClass superklass = klass.getSTClass();

        if (superklass != null && superklass.getScope() != null) {
            _getUnknownPrimitives(primitives, superklass.getScope());
        }

        return primitives;
    }

    public void test() {
        List<STSymbol> primitives = new ArrayList<STSymbol>();
        List<STObject> objects = ImageSuite.image().asList();
        for (STObject obj : objects) {
            if (obj instanceof STClass == false) {
                continue;
            }
            STClass klass = (STClass) obj;
            getUnknownPrimitives(klass, primitives);
        }

        System.out.println(primitives);
    }
}
