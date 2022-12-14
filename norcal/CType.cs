using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

enum CTypeTag
{
    Simple,
    Pointer,
    Struct,
    Union,
    Array,
    ArrayWithDimensionExpression,
}

enum CSimpleType
{
    Implied,
    Void,
    UInt8,
    UInt16,
}

[DebuggerDisplay("{Show(),nq}")]
class CType : IEquatable<CType>
{
    public CTypeTag Tag;
    public CSimpleType SimpleType;
    public string Name;
    public CType Subtype;
    public int Dimension;
    public Expr DimensionExpression;

    public static readonly CType Implied = MakeSimple(CSimpleType.Implied);
    public static readonly CType Void = MakeSimple(CSimpleType.Void);
    public static readonly CType UInt8 = MakeSimple(CSimpleType.UInt8);
    public static readonly CType UInt8Ptr = MakePointer(UInt8);
    public static readonly CType UInt16 = MakeSimple(CSimpleType.UInt16);
    public static readonly CType UInt16Ptr = MakePointer(UInt16);

    public static CType MakeSimple(CSimpleType simple)
    {
        return new CType
        {
            Tag = CTypeTag.Simple,
            SimpleType = simple,
        };
    }

    public static CType MakePointer(CType subtype)
    {
        return new CType
        {
            Tag = CTypeTag.Pointer,
            Subtype = subtype,
        };
    }

    public static CType MakeStruct(string name)
    {
        return new CType
        {
            Tag = CTypeTag.Struct,
            Name = name,
        };
    }

    public static CType MakeUnion(string name)
    {
        return new CType
        {
            Tag = CTypeTag.Union,
            Name = name,
        };
    }

    public static CType MakeArray(CType elementType, int dimension)
    {
        return new CType
        {
            Tag = CTypeTag.Array,
            Subtype = elementType,
            Dimension = dimension,
        };
    }

    public static CType MakeArray(CType elementType, Expr dimension)
    {
        return new CType
        {
            Tag = CTypeTag.ArrayWithDimensionExpression,
            Subtype = elementType,
            DimensionExpression = dimension,
        };
    }

    public bool IsSimple => Tag == CTypeTag.Simple;
    public bool IsPointer => Tag == CTypeTag.Pointer;
    public bool IsStructOrUnion => Tag == CTypeTag.Struct || Tag == CTypeTag.Union;
    public bool IsArray => Tag == CTypeTag.Array || Tag == CTypeTag.ArrayWithDimensionExpression;
    public bool IsInteger => IsSimple && (SimpleType == CSimpleType.UInt8 || SimpleType == CSimpleType.UInt16);

    public bool IsUnsigned => IsInteger && (SimpleType == CSimpleType.UInt8 || SimpleType == CSimpleType.UInt16);

    public override bool Equals(object obj)
    {
        // Don't bother supporting equality-testing with arbitrary other types.
        throw new NotSupportedException();
    }

    public override int GetHashCode()
    {
        throw new NotSupportedException();
    }

    public bool Equals(CType other)
    {
        if (Tag != other.Tag) return false;
        else if (Tag == CTypeTag.Simple) return SimpleType.Equals(other.SimpleType);
        else if (Tag == CTypeTag.Pointer || Tag == CTypeTag.Array || Tag == CTypeTag.ArrayWithDimensionExpression) return Subtype.Equals(other.Subtype);
        else if (Tag == CTypeTag.Struct) return Name == other.Name;
        else
        {
            Program.NYI();
            return false;
        }
    }

    public static bool operator ==(CType a, CType b) => a.Equals(b);
    public static bool operator !=(CType a, CType b) => !a.Equals(b);

    public string Show()
    {
        if (Tag == CTypeTag.Simple) return SimpleType.ToString().ToLower();
        else if (Tag == CTypeTag.Pointer) return Subtype.Show() + "*";
        else if (Tag == CTypeTag.Struct) return "struct " + Name;
        else if (Tag == CTypeTag.Union) return "union " + Name;
        else if (Tag == CTypeTag.Array) return string.Format("{0}[{1}]", Subtype.Show(), Dimension);
        else if (Tag == CTypeTag.ArrayWithDimensionExpression) return string.Format("{0}[{1}]", Subtype.Show(), DimensionExpression.Show());
        else throw new NotImplementedException();
    }
}
