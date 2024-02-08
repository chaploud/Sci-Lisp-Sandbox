use id_arena::Id;

pub type FunctionDefId = Id<FunctionDef>;

pub struct FunctionDef {
    pub id: Option<FunctionDefId>,
    pub package_id: Option<PackageId>,
    pub module_id: Option<ModuleId>,
    pub file_id: Option<FileId>,
    pub ast: Arc<ast::Function>,
    
}
