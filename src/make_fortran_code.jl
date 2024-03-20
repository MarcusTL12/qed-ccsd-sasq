include("../../TensorOperation-eT-code/src/omeinsum_impl.jl")

function make_code_from_dir(dir, outdir)
    for name in readdir(dir)
        f = include("$dir/$name")

        namename = split(name, ".")[1]

        open("$outdir/$namename.F90", "w") do io
            print(io, f)
        end
    end
end
