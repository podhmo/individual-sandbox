package structopt

import (
	"fmt"
	"os"
	"reflect"
	"strings"
	"time"
	"unsafe"

	flag "github.com/spf13/pflag"
)

// TODO: nested
// TODO: map
// TODO: embed

type Builder struct {
	Name         string
	HandlingMode flag.ErrorHandling

	EnvvarSupport bool
	EnvPrefix     string
	EnvNameFunc   func(string) string

	FlagnameTag  string
	ShorthandTag string
	HelpTextTag  string
}

func NewBuilder() *Builder {
	name := os.Args[0]
	b := &Builder{
		Name:          name,
		FlagnameTag:   "json",
		ShorthandTag:  "short",
		HelpTextTag:   "help",
		EnvvarSupport: true,
		HandlingMode:  flag.ExitOnError,
	}

	if v := os.Getenv("ENV_PREFIX"); v != "" {
		b.EnvPrefix = v
	}
	b.EnvNameFunc = func(name string) string {
		return b.EnvPrefix + strings.ReplaceAll(strings.ToUpper(name), "-", "_")
	}
	return b
}

type FlagSet struct {
	*flag.FlagSet
	builder *Builder
}

var (
	rTimeDuration = reflect.TypeOf(time.Second)
)

func (b *Builder) Build(o interface{}) *FlagSet {
	rt := reflect.TypeOf(o)
	rv := reflect.ValueOf(o)

	if rt.Kind() != reflect.Ptr {
		panic(fmt.Sprintf("%v is not pointer of struct", rt)) // for canAddr
	}
	rt = rt.Elem()
	rv = rv.Elem()

	name := b.Name
	if name == "" {
		name = rt.Name()
	}
	fs := flag.NewFlagSet(name, b.HandlingMode)

	for i := 0; i < rt.NumField(); i++ {
		rf := rt.Field(i)
		if !rf.IsExported() {
			continue
		}

		// todo: custom type
		fieldname := rf.Name
		if v, ok := rf.Tag.Lookup(b.FlagnameTag); ok {
			fieldname = v
		}
		helpText := "-"
		if v, ok := rf.Tag.Lookup(b.HelpTextTag); ok {
			helpText = v
		}
		if b.EnvvarSupport {
			helpText = fmt.Sprintf("ENV: %s\t", b.EnvNameFunc(fieldname)) + helpText
		}
		shorthand := ""
		if v, ok := rf.Tag.Lookup(b.ShorthandTag); ok {
			shorthand = v
		}

		fv := rv.Field(i)

		switch rf.Type.Kind() {
		case reflect.Bool:
			ref := (*bool)(unsafe.Pointer(fv.UnsafeAddr()))
			fs.BoolVarP(ref, fieldname, shorthand, fv.Bool(), helpText)
		case reflect.Float64:
			ref := (*float64)(unsafe.Pointer(fv.UnsafeAddr()))
			fs.Float64VarP(ref, fieldname, shorthand, fv.Float(), helpText)
		case reflect.Int64:
			switch rf.Type {
			case rTimeDuration:
				ref := (*time.Duration)(unsafe.Pointer(fv.UnsafeAddr()))
				fs.DurationVarP(ref, fieldname, shorthand, time.Duration(fv.Int()), helpText)
			default:
				ref := (*int64)(unsafe.Pointer(fv.UnsafeAddr()))
				fs.Int64VarP(ref, fieldname, shorthand, fv.Int(), helpText)
			}
		case reflect.Int:
			ref := (*int)(unsafe.Pointer(fv.UnsafeAddr()))
			fs.IntVarP(ref, fieldname, shorthand, int(fv.Int()), helpText)
		case reflect.String:
			ref := (*string)(unsafe.Pointer(fv.UnsafeAddr()))
			fs.StringVarP(ref, fieldname, shorthand, fv.String(), helpText)
		case reflect.Uint64:
			ref := (*uint64)(unsafe.Pointer(fv.UnsafeAddr()))
			fs.Uint64VarP(ref, fieldname, shorthand, fv.Uint(), helpText)
		case reflect.Uint:
			ref := (*uint)(unsafe.Pointer(fv.UnsafeAddr()))
			fs.UintVarP(ref, fieldname, shorthand, uint(fv.Uint()), helpText)
		case reflect.Slice:
			switch rf.Type.Elem().Kind() {
			case reflect.Bool:
				var defaultValue []bool
				for i := 0; i < fv.Len(); i++ {
					defaultValue = append(defaultValue, fv.Index(i).Bool())
				}
				ref := (*[]bool)(unsafe.Pointer(fv.UnsafeAddr()))
				fs.BoolSliceVarP(ref, fieldname, shorthand, defaultValue, helpText)
			case reflect.Float64:
				var defaultValue []float64
				for i := 0; i < fv.Len(); i++ {
					defaultValue = append(defaultValue, fv.Index(i).Float())
				}
				ref := (*[]float64)(unsafe.Pointer(fv.UnsafeAddr()))
				fs.Float64SliceVarP(ref, fieldname, shorthand, defaultValue, helpText)
			case reflect.Int64:
				switch rf.Type.Elem() {
				case rTimeDuration:
					ref := (*[]time.Duration)(unsafe.Pointer(fv.UnsafeAddr()))
					var defaultValue []time.Duration
					for i := 0; i < fv.Len(); i++ {
						defaultValue = append(defaultValue, time.Duration(fv.Index(i).Int()))
					}
					fs.DurationSliceVarP(ref, fieldname, shorthand, defaultValue, helpText)
				default:
					var defaultValue []int64
					for i := 0; i < fv.Len(); i++ {
						defaultValue = append(defaultValue, fv.Index(i).Int())
					}
					ref := (*[]int64)(unsafe.Pointer(fv.UnsafeAddr()))
					fs.Int64SliceVarP(ref, fieldname, shorthand, defaultValue, helpText)
				}
			case reflect.Int:
				var defaultValue []int
				for i := 0; i < fv.Len(); i++ {
					defaultValue = append(defaultValue, int(fv.Index(i).Int()))
				}
				ref := (*[]int)(unsafe.Pointer(fv.UnsafeAddr()))
				fs.IntSliceVarP(ref, fieldname, shorthand, defaultValue, helpText)
			case reflect.String:
				var defaultValue []string
				for i := 0; i < fv.Len(); i++ {
					defaultValue = append(defaultValue, fv.Index(i).String())
				}
				ref := (*[]string)(unsafe.Pointer(fv.UnsafeAddr()))
				fs.StringSliceVarP(ref, fieldname, shorthand, defaultValue, helpText)
			case reflect.Uint:
				var defaultValue []uint
				for i := 0; i < fv.Len(); i++ {
					defaultValue = append(defaultValue, uint(fv.Index(i).Uint()))
				}
				ref := (*[]uint)(unsafe.Pointer(fv.UnsafeAddr()))
				fs.UintSliceVarP(ref, fieldname, shorthand, defaultValue, helpText)
			// case reflect.Uint64:
			default:
				panic(fmt.Sprintf("unsupported slice type %v", rf.Type))
			}
		default:
			// TODO: map
			panic(fmt.Sprintf("unsupported type %v", rf.Type))
		}
	}
	return &FlagSet{FlagSet: fs, builder: b}
}

func (fs *FlagSet) Parse(args []string) {
	fs.FlagSet.Parse(args)
	if fs.builder.EnvvarSupport {
		fs.FlagSet.VisitAll(func(f *flag.Flag) {
			envname := fs.builder.EnvNameFunc(f.Name)
			if v := os.Getenv(envname); v != "" {
				if err := fs.Set(f.Name, v); err != nil {
					panic(fmt.Sprintf("on envvar %s=%v, %+v", envname, v, err))
				}
			}
		})
	}
}
