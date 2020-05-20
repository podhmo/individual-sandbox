// Code generated by protoc-gen-go. DO NOT EDIT.
// versions:
// 	protoc-gen-go v1.22.0
// 	protoc        v3.11.4
// source: xxx.proto

package myapp

import (
	proto "github.com/golang/protobuf/proto"
	protoreflect "google.golang.org/protobuf/reflect/protoreflect"
	protoimpl "google.golang.org/protobuf/runtime/protoimpl"
	reflect "reflect"
	sync "sync"
)

const (
	// Verify that this generated code is sufficiently up-to-date.
	_ = protoimpl.EnforceVersion(20 - protoimpl.MinVersion)
	// Verify that runtime/protoimpl is sufficiently up-to-date.
	_ = protoimpl.EnforceVersion(protoimpl.MaxVersion - 20)
)

// This is a compile-time assertion that a sufficiently up-to-date version
// of the legacy proto package is being used.
const _ = proto.ProtoPackageIsVersion4

type X struct {
	state         protoimpl.MessageState
	sizeCache     protoimpl.SizeCache
	unknownFields protoimpl.UnknownFields

	Name string `protobuf:"bytes,1,opt,name=name,proto3" json:"name,omitempty"`
}

func (x *X) Reset() {
	*x = X{}
	if protoimpl.UnsafeEnabled {
		mi := &file_xxx_proto_msgTypes[0]
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		ms.StoreMessageInfo(mi)
	}
}

func (x *X) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*X) ProtoMessage() {}

func (x *X) ProtoReflect() protoreflect.Message {
	mi := &file_xxx_proto_msgTypes[0]
	if protoimpl.UnsafeEnabled && x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use X.ProtoReflect.Descriptor instead.
func (*X) Descriptor() ([]byte, []int) {
	return file_xxx_proto_rawDescGZIP(), []int{0}
}

func (x *X) GetName() string {
	if x != nil {
		return x.Name
	}
	return ""
}

type Y struct {
	state         protoimpl.MessageState
	sizeCache     protoimpl.SizeCache
	unknownFields protoimpl.UnknownFields

	Id   uint64 `protobuf:"varint,1,opt,name=id,proto3" json:"id,omitempty"`
	Name string `protobuf:"bytes,2,opt,name=name,proto3" json:"name,omitempty"`
}

func (x *Y) Reset() {
	*x = Y{}
	if protoimpl.UnsafeEnabled {
		mi := &file_xxx_proto_msgTypes[1]
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		ms.StoreMessageInfo(mi)
	}
}

func (x *Y) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*Y) ProtoMessage() {}

func (x *Y) ProtoReflect() protoreflect.Message {
	mi := &file_xxx_proto_msgTypes[1]
	if protoimpl.UnsafeEnabled && x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use Y.ProtoReflect.Descriptor instead.
func (*Y) Descriptor() ([]byte, []int) {
	return file_xxx_proto_rawDescGZIP(), []int{1}
}

func (x *Y) GetId() uint64 {
	if x != nil {
		return x.Id
	}
	return 0
}

func (x *Y) GetName() string {
	if x != nil {
		return x.Name
	}
	return ""
}

type Node struct {
	state         protoimpl.MessageState
	sizeCache     protoimpl.SizeCache
	unknownFields protoimpl.UnknownFields

	// Types that are assignable to Node:
	//	*Node_X
	//	*Node_Y
	Node isNode_Node `protobuf_oneof:"node"`
}

func (x *Node) Reset() {
	*x = Node{}
	if protoimpl.UnsafeEnabled {
		mi := &file_xxx_proto_msgTypes[2]
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		ms.StoreMessageInfo(mi)
	}
}

func (x *Node) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*Node) ProtoMessage() {}

func (x *Node) ProtoReflect() protoreflect.Message {
	mi := &file_xxx_proto_msgTypes[2]
	if protoimpl.UnsafeEnabled && x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use Node.ProtoReflect.Descriptor instead.
func (*Node) Descriptor() ([]byte, []int) {
	return file_xxx_proto_rawDescGZIP(), []int{2}
}

func (m *Node) GetNode() isNode_Node {
	if m != nil {
		return m.Node
	}
	return nil
}

func (x *Node) GetX() *X {
	if x, ok := x.GetNode().(*Node_X); ok {
		return x.X
	}
	return nil
}

func (x *Node) GetY() *Y {
	if x, ok := x.GetNode().(*Node_Y); ok {
		return x.Y
	}
	return nil
}

type isNode_Node interface {
	isNode_Node()
}

type Node_X struct {
	X *X `protobuf:"bytes,1,opt,name=x,proto3,oneof"`
}

type Node_Y struct {
	Y *Y `protobuf:"bytes,2,opt,name=y,proto3,oneof"`
}

func (*Node_X) isNode_Node() {}

func (*Node_Y) isNode_Node() {}

var File_xxx_proto protoreflect.FileDescriptor

var file_xxx_proto_rawDesc = []byte{
	0x0a, 0x09, 0x78, 0x78, 0x78, 0x2e, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x12, 0x05, 0x6d, 0x79, 0x61,
	0x70, 0x70, 0x22, 0x17, 0x0a, 0x01, 0x58, 0x12, 0x12, 0x0a, 0x04, 0x6e, 0x61, 0x6d, 0x65, 0x18,
	0x01, 0x20, 0x01, 0x28, 0x09, 0x52, 0x04, 0x6e, 0x61, 0x6d, 0x65, 0x22, 0x27, 0x0a, 0x01, 0x59,
	0x12, 0x0e, 0x0a, 0x02, 0x69, 0x64, 0x18, 0x01, 0x20, 0x01, 0x28, 0x04, 0x52, 0x02, 0x69, 0x64,
	0x12, 0x12, 0x0a, 0x04, 0x6e, 0x61, 0x6d, 0x65, 0x18, 0x02, 0x20, 0x01, 0x28, 0x09, 0x52, 0x04,
	0x6e, 0x61, 0x6d, 0x65, 0x22, 0x42, 0x0a, 0x04, 0x4e, 0x6f, 0x64, 0x65, 0x12, 0x18, 0x0a, 0x01,
	0x78, 0x18, 0x01, 0x20, 0x01, 0x28, 0x0b, 0x32, 0x08, 0x2e, 0x6d, 0x79, 0x61, 0x70, 0x70, 0x2e,
	0x58, 0x48, 0x00, 0x52, 0x01, 0x78, 0x12, 0x18, 0x0a, 0x01, 0x79, 0x18, 0x02, 0x20, 0x01, 0x28,
	0x0b, 0x32, 0x08, 0x2e, 0x6d, 0x79, 0x61, 0x70, 0x70, 0x2e, 0x59, 0x48, 0x00, 0x52, 0x01, 0x79,
	0x42, 0x06, 0x0a, 0x04, 0x6e, 0x6f, 0x64, 0x65, 0x42, 0x09, 0x5a, 0x07, 0x2e, 0x3b, 0x6d, 0x79,
	0x61, 0x70, 0x70, 0x62, 0x06, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x33,
}

var (
	file_xxx_proto_rawDescOnce sync.Once
	file_xxx_proto_rawDescData = file_xxx_proto_rawDesc
)

func file_xxx_proto_rawDescGZIP() []byte {
	file_xxx_proto_rawDescOnce.Do(func() {
		file_xxx_proto_rawDescData = protoimpl.X.CompressGZIP(file_xxx_proto_rawDescData)
	})
	return file_xxx_proto_rawDescData
}

var file_xxx_proto_msgTypes = make([]protoimpl.MessageInfo, 3)
var file_xxx_proto_goTypes = []interface{}{
	(*X)(nil),    // 0: myapp.X
	(*Y)(nil),    // 1: myapp.Y
	(*Node)(nil), // 2: myapp.Node
}
var file_xxx_proto_depIdxs = []int32{
	0, // 0: myapp.Node.x:type_name -> myapp.X
	1, // 1: myapp.Node.y:type_name -> myapp.Y
	2, // [2:2] is the sub-list for method output_type
	2, // [2:2] is the sub-list for method input_type
	2, // [2:2] is the sub-list for extension type_name
	2, // [2:2] is the sub-list for extension extendee
	0, // [0:2] is the sub-list for field type_name
}

func init() { file_xxx_proto_init() }
func file_xxx_proto_init() {
	if File_xxx_proto != nil {
		return
	}
	if !protoimpl.UnsafeEnabled {
		file_xxx_proto_msgTypes[0].Exporter = func(v interface{}, i int) interface{} {
			switch v := v.(*X); i {
			case 0:
				return &v.state
			case 1:
				return &v.sizeCache
			case 2:
				return &v.unknownFields
			default:
				return nil
			}
		}
		file_xxx_proto_msgTypes[1].Exporter = func(v interface{}, i int) interface{} {
			switch v := v.(*Y); i {
			case 0:
				return &v.state
			case 1:
				return &v.sizeCache
			case 2:
				return &v.unknownFields
			default:
				return nil
			}
		}
		file_xxx_proto_msgTypes[2].Exporter = func(v interface{}, i int) interface{} {
			switch v := v.(*Node); i {
			case 0:
				return &v.state
			case 1:
				return &v.sizeCache
			case 2:
				return &v.unknownFields
			default:
				return nil
			}
		}
	}
	file_xxx_proto_msgTypes[2].OneofWrappers = []interface{}{
		(*Node_X)(nil),
		(*Node_Y)(nil),
	}
	type x struct{}
	out := protoimpl.TypeBuilder{
		File: protoimpl.DescBuilder{
			GoPackagePath: reflect.TypeOf(x{}).PkgPath(),
			RawDescriptor: file_xxx_proto_rawDesc,
			NumEnums:      0,
			NumMessages:   3,
			NumExtensions: 0,
			NumServices:   0,
		},
		GoTypes:           file_xxx_proto_goTypes,
		DependencyIndexes: file_xxx_proto_depIdxs,
		MessageInfos:      file_xxx_proto_msgTypes,
	}.Build()
	File_xxx_proto = out.File
	file_xxx_proto_rawDesc = nil
	file_xxx_proto_goTypes = nil
	file_xxx_proto_depIdxs = nil
}