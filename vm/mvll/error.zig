pub const Error = opaque {
    pub const TypeId = *anyopaque;

    extern fn LLVMGetErrorTypeId(err: *Error) TypeId;
    pub const typeId = LLVMGetErrorTypeId;

    extern fn LLVMConsumeError(err: *Error) void;
    pub const consume = LLVMConsumeError;

    extern fn LLVMGetErrorMessage(err: *Error) [*:0]const u8;
    pub const message = LLVMGetErrorMessage;

    extern fn LLVMDisposeErrorMessage(msg: [*:0]const u8) void;
    pub const disposeMessage = LLVMDisposeErrorMessage;

    extern fn LLVMCreateStringError(msg: [*:0]const u8) *Error;
    pub const init = LLVMCreateStringError;
};
